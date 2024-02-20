#' R Module for the Biodiversity Metrices Tab Panel.
#'
#' @description
#' R Module for the Biodiversity Metrices Tab Panel .
#'
#' @import shiny
#' @rdname mod_bio_matrix

mod_bio_matrix_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    useShinyjs(),
    title = "Metrices",
    br(), div(style = "border:1px solid black;", "Number of Occurences by Month", withSpinner(
      plotlyOutput(ns("bar1"), height = "100%", width = "90%"),
      type = 4,
      color = "#2E8B57",
      size = 0.7
    )), br(), br(), br(), br(), div(style = "border:1px solid black;", "Number of Occurences per Basis of Record", withSpinner(
      plotlyOutput(ns("pie1"), height = "100%", width = "90%"),
      type = 4,
      color = "#2E8B57",
      size = 0.7
    )), br(), br(), br(), br(), br(), div(style = "border:1px solid black;", "Number of Occurences by Year", withSpinner(
      plotlyOutput(ns("line1"), height = "100%", width = "90%"),
      type = 4,
      color = "#2E8B57",
      size = 0.7
    )), br(), br(), br(), br(), br(), div(style = "border:1px solid black;", "Number of Occurences by License", withSpinner(
      plotlyOutput(ns("bar2"), height = "100%", width = "90%"),
      type = 4,
      color = "#2E8B57",
      size = 0.7
    )), br(), br(), br(), br(), br(), div(style = "border:1px solid black;", "Total Number of Occurences by Dataset", withSpinner(
      plotlyOutput(ns("bar3"), height = "100%", width = "90%"),
      type = 4,
      color = "#2E8B57",
      size = 0.7
    )), br(),
  )
}


mod_bio_matrix_server <- function(id) {
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

    # For metrices

    output$bar1 <- renderPlotly({
      bar <- occd() %>%
        mutate(month = format(`Event Date`, format = "%B"))


      bar1 <- bio_get_count(
        df = bar,
        group_var = month
      )



      plotly::plot_ly(
        data = bar1,
        x = ~month,
        y = ~n,
        type = "bar",
        marker = list(color = "cyan")
      ) %>%
        plotly::layout(
          yaxis = list(title = "Total Number of Occurences"),
          xaxis = list(title = "Month"),
          hovermode = "compare",
          margin = list(
            # l = 60,
            # r = 40,
            b = 10,
            t = 10,
            pad = 2
          )
        )
    })

    output$bar2 <- renderPlotly({
      bar2 <- occd()

      bar2 <- bio_get_count(
        df = bar2,
        group_var = License
      )


      plotly::plot_ly(
        data = bar2,
        x = ~License,
        y = ~n,
        type = "bar",
        marker = list(color = "yellow")
      ) %>%
        plotly::layout(
          yaxis = list(title = "Total No of Occurences"),
          xaxis = list(title = "License"),
          hovermode = "compare",
          margin = list(
            # l = 60,
            # r = 40,
            b = 10,
            t = 10,
            pad = 2
          )
        )
    })

    output$bar3 <- renderPlotly({
      bar3 <- occd()


      bar3 <- bio_get_count(
        df = bar3,
        group_var = `Collection Code`
      )


      plotly::plot_ly(
        data = bar3,
        x = ~`Collection Code`,
        y = ~n,
        type = "bar",
        marker = list(color = "purple")
      ) %>%
        plotly::layout(
          yaxis = list(title = "Total No of Occurences"),
          xaxis = list(title = "Dataset Type"),
          hovermode = "compare",
          margin = list(
            # l = 60,
            # r = 40,
            b = 10,
            t = 10,
            pad = 2
          )
        )
    })


    output$pie1 <- renderPlotly({
      pie1 <- occd()

      pie1 <- bio_get_count(
        df = pie1,
        group_var = `Basis Of Record`
      )


      plotly::plot_ly(
        data = pie1,
        labels = ~`Basis Of Record`,
        values = ~n,
        type = "pie"
      ) %>%
        plotly::layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })

    output$line1 <- renderPlotly({
      line <- occd() %>%
        mutate(year = format(`Event Date`, format = "%Y"))

      line1 <- sqldf("select distinct year,count(*) as totalocc from line
              group by year
              order by year")
      plot_ly(data = line1) %>%
        plotly::add_trace(
          x = ~year,
          y = ~totalocc,
          type = "scatter",
          mode = "lines+markers"
        ) %>%
        plotly::layout(
          title = "",
          yaxis = list(title = "Total Number of Occurences"),
          xaxis = list(title = "Year"),
          hovermode = "compare"
        )
    })
  })
}
