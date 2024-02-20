#' R Module for the Biodiversity Table Panel.
#'
#' @description
#' R Module for the Biodiversity Table Panel .
#'
#' @import shiny
#' @rdname mod_bio_table

mod_bio_table_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    useShinyjs(),
    title = "Table",
    div(
      div(column(
        width = 4,
        shinyWidgets::pickerInput(
          inputId = ns("colselect"), label = "Select Columns:",
          choices = c("Scientific Name", "Country", "Coordinates"),
          multiple = TRUE,
          selected = NULL,
          options = shinyWidgets::pickerOptions(
            actionsBox = TRUE,
            title = "Select Columns to Display",
          ), width = "100%"
        )
      )), br(), br(), br(),
      div(
        withSpinner(
          dataTableOutput(ns("data_table")),
          type = 4,
          color = "#2E8B57",
          size = 0.7
        )
      )
    )
  )
}


mod_bio_table_server <- function(id) {
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


    observe({
      shinyWidgets::updatePickerInput(session, "colselect",
        choices = c(names(occd())),
        selected = c(names(occd())[names(occd()) %in% c("Scientific Name", "Country", "Coordinates", "Event Date", "`Basis Of Record`", "`Occurrence Identifier`", "Kingdom", "Higher Classification", "Individual Count", "Family", "Taxon Rank")]),
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          title = "Select Columns to Display",
          header = "Select Columns"
        ),
        choicesOpt = list(
          style = rep(("color: black; background: lightgrey; font-weight: bold;"), 1000)
        )
      )
    })


    observeEvent(input$colselect, {
      # Output Table
      output$data_table <- renderDataTable({
        occd() %>%
          select(one_of(input$colselect)) %>%
          datatable(filter = "top", extensions = "Buttons", options = list(
            scrollX = TRUE,
            pageLength = 25, autoWidth = FALSE,
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel", "pdf")
          ))
      })
    })

    observeEvent(input$reset, {
      shinyjs::reset("sidebar")
    })
  })
}
