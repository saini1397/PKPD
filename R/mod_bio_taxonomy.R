#' Biodiversity Taxonomy Panel Output
#'
#' @description
#' Biodiversity Taxonomy Panel Output
#'
#' @import shiny
#' @rdname mod_bio_taxonomy

mod_bio_taxonomy_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    useShinyjs(),
    title = "Taxonomy",
    br(), div(style = "border:1px solid black;", "TAXONOMIC DISTRIBUTION OF OCCURRENCES", withSpinner(
      highchartOutput(ns("tax1"), height = "100%", width = "90%"),
      type = 4,
      color = "#2E8B57",
      size = 0.7
    )),
    br(), br(), br(), br(), div(
      style = "border:1px solid black;", "TAXONOMIC DISTRIBUTION TABLE OF OCCURRENCES",
      div(column(width = 4, br(), "Select Grouping Category from the Dropdown: "), column(width = 4, selectInput(
        inputId = ns("tax3in"), label = "",
        choices = c("Scientific Name", "Kingdom", "Class", "Family", "Species", "Overall"),
        selected = "Overall"
      ))),
      div(withSpinner(
        dataTableOutput(ns("tax2"), height = "100%", width = "90%"),
        type = 4,
        color = "#2E8B57",
        size = 0.7
      ))
    )
  )
}


mod_bio_taxonomy_server <- function(id) {
  moduleServer(id = id, module = function(input, output, session) {
    occd <- reactive({
      occ %>%
        filter(License %in% input$s2) %>%
        filter(`Scientific Name` %in% input$s3) %>%
        filter(`Basis Of Record` %in% input$s4) %>%
        filter(`Event Date` %in% input$s5) %>%
        filter(between(`Latitude Decimal`, min(`Latitude Decimal`), input$s6)) %>%
        filter(between(`Longitude Decimal`, min(`Longitude Decimal`), input$s61)) %>%
        filter(`Administrative Area` %in% input$s7) %>%
        filter(Country %in% input$s8) %>%
        filter(Continent %in% input$s9) %>%
        filter(`Rights Holder` %in% input$s10)
    })

    # Taxonomy table.


    taxt <- reactive({
      if (grepl("Scientific", input$tax3in)) {
        taxdata <- occd()

        bio_get_count(
          df = taxdata,
          group_var = `Scientific Name`
        )
      } else if (grepl("Kingdom", input$tax3in)) {
        taxdata <- occd()

        bio_get_count(
          df = taxdata,
          group_var = Kingdom
        )
      } else if (grepl("Class", input$tax3in)) {
        taxdata <- occd()
        bio_get_count(
          df = taxdata,
          group_var = Class
        )
      } else if (grepl("Family", input$tax3in)) {
        taxdata <- occd()
        bio_get_count(
          df = taxdata,
          group_var = Family
        )
      } else if (grepl("Species", input$tax3in)) {
        taxdata <- occd()

        bio_get_count(
          df = taxdata,
          group_var = `Taxon Rank`
        )
      } else if (grepl("Overall", input$tax3in)) {
        occd() %>%
          group_by(Kingdom, Class, Family, `Scientific Name`, `Taxon Rank`) %>%
          summarise(n = n()) %>%
          arrange(desc(n)) %>% 
          ungroup()
      }
    })

    observeEvent(input$tax3in, {
      output$tax2 <- renderDataTable({
        taxt() %>%
          rename(Occurences = n) %>%
          datatable(filter = "top", extensions = "Buttons", options = list(
            scrollX = TRUE,
            pageLength = 25,
            autoWidth = FALSE,
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel", "pdf")
          ))
      })
    })

    # End of taxonomy table



    # taxonomy PIE charct with drill down

    output$tax1 <- renderHighchart({
      # subset columns (not all needed)
      df <- occd() %>%
        select(Kingdom, Class, Family, `Scientific Name`, `Vernacular Name`)


      by_type <- df %>%
        group_by(Kingdom) %>%
        summarise(occurences = n())

      pie_chart <- by_type %>%
        # set up highchart object
        hchart("pie",
          # mapping for pie chart
          hcaes(x = Kingdom, y = occurences, drilldown = Kingdom),
          name = "Occurences"
        ) %>%
        hc_title(text = "By Kingdom Type")


      donut_chart <- pie_chart %>%
        hc_plotOptions(pie = list(innerSize = "70%"))


      by_subtype1 <- df %>%
        group_by(Kingdom, Class) %>%
        summarise(occurences = n()) %>%
        # create nested data at parent level
        group_nest(Kingdom) %>%
        mutate(
          # id should be set to parent level
          id = Kingdom,
          # type specifies chart type
          type = "column",
          # drilldown data should contain arguments for chart - use purrr to map
          data = purrr::map(data, mutate, name = Class, y = occurences),
          data = purrr::map(data, list_parse)
        )

      drilldown_chart <- donut_chart %>%
        hc_drilldown(
          # map to data
          series = list_parse(by_subtype1),
          allowPointDrilldown = TRUE,
          # set stylings of data labels that offer drill down views
          activeDataLabelStyle = list(
            textDecoration = "none",
            color = "black"
          )
        )


      final_chart <- drilldown_chart %>%
        # relabel x Axis
        hc_xAxis(title = list(text = "Type")) %>%
        # relabel y Axis
        hc_yAxis(title = list(text = "# of Occurences")) %>%
        # reorder column charts by y Axis
        hc_plotOptions(column = list(
          dataSorting = list(enabled = TRUE)
        )) %>%
        # customize drilldown & drillup events
        hc_chart(
          events = list(
            drilldown = JS(
              "function(){
               this.title.update({text: 'By Class Sub Type'})
               this.update({
                  xAxis:{visible:true},
                  yAxis:{visible:true}
               })
               }"
            ),
            drillup = JS("function() {
              this.title.update({text: 'By Kingdom name Type'})
              this.update({
                xAxis:{visible:false},
                yAxis:{visible:false}
               })
             }")
          )
        )

      final_chart
    })
  })
}
