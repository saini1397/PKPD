server <- (function(input, output, session) {
  x1 <- reactive({
    get(input$s1)
  })


  x2 <- reactive({
    get(input$s2)
  })

  x3 <- reactive({
    get(input$s3)
  })



  colvars <- reactive({
    if (length(names(x1())[names(x1()) %in% c("PARAMCD")]) != 0) {
      c("STUDYID", "USUBJID", "SUBJID", "PARAM", "PARAMCD", "AVAL")
    } else {
      names(x1())[1:10]
    }
  })


  ################## Listing Purpose
  # To update drop down dynamically based on platform selected
  observe({
    shinyWidgets::updatePickerInput(session, "colselect",
      choices = unique(names(x1())),
      selected = colvars(),
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        title = "Please select Columns to Display",
        header = "This is a Listing"
      ),
      choicesOpt = list(
        style = rep(("color: black; background: lightgrey; font-weight: bold;"), 1000)
      )
    )
  })


  observeEvent(input$colselect, {
    # Output Table
    output$data_table <- renderDataTable({
      x1() %>%
        select(one_of(input$colselect)) %>%
        datatable(filter = "top", extensions = "Buttons", options = list(
          scrollX = TRUE,
          pageLength = 25, autoWidth = FALSE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel", "pdf")
        ))
    })
  })


  observe({
    updateSelectInput(session, "param1",
      choices = c(names(x1())),
      selected = "PARAM"
    )

    updateSelectInput(session, "visit1",
      choices = c(names(x1())),
      selected = "VISITNUM"
    )

    updateSelectInput(session, "time1",
      choices = c(names(x1())),
      selected = c(names(x1()))[186]
    )



    updateSelectInput(session, "param2",
      choices = c(names(x1())),
      selected = "PARAM"
    )

    updateSelectInput(session, "visit2",
      choices = c(names(x1())),
      selected = "VISITNUM"
    )
  })


  summ <- reactive({
    req(x1())
    x1() %>%
      group_by(TRT01AN, TRT01A, .data[[input$param1]], .data[[input$visit1]], .data[[input$time1]]) %>%
      summarise(
        mean = round(mean(AVAL), 2),
        median = round(median(AVAL), 2),
        min = round(min(AVAL), 1),
        max = round(max(AVAL), 1),
        sd = round(sd(AVAL), 3),
        lclm = round(mean(AVAL) - (1.96 * sd(AVAL) / sqrt(length(AVAL))), 3),
        uclm = round(mean(AVAL) + (1.96 * sd(AVAL) / sqrt(length(AVAL))), 3)
      ) %>%
      ungroup() %>%
      arrange(TRT01AN, TRT01A, .data[[input$param1]], .data[[input$visit1]], .data[[input$time1]]) %>%
      select(
        TRT01A, .data[[input$param1]], .data[[input$visit1]], .data[[input$time1]],
        min, max, median, mean, sd, lclm, uclm
      )
  })

  observeEvent(session, {
    output$data_table2 <- renderDataTable({
      summ() %>%
        datatable(filter = "top", extensions = "Buttons", options = list(
          scrollX = TRUE,
          pageLength = 25, autoWidth = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel", "pdf")
        ))

      setnames(summ(), c(
        "Treatment Arm", "Parameter", "Visit", "Time Point", "Min.", "Max.", "Median",
        "Mean", "STD.", "LCLM", "UCLM"
      ))
    })
  })


  fig <- reactive({
    req(x1())


    x1() %>%
      group_by(SUBJID, TRT01AN, TRT01A, .data[[input$param1]], ADY) %>%
      summarise(mean = round(mean(AVAL),2)) %>%
      arrange(TRT01AN, TRT01A, .data[[input$param1]]) %>%
      select(SUBJID, TRT01A, .data[[input$param1]], mean, ADY)
  })

  observeEvent(session, {
    output$chart2 <- renderHighchart({
      fig() %>%
        mutate(mean = ifelse(is.na(mean), 1, mean)) %>%
        hchart(
          "line", hcaes(x = ADY, y = mean, group = c(SUBJID))
        ) %>%
        hc_exporting(
          enabled = TRUE,
          buttons = list(
            customButton = list(
              text = "Linear",
              onclick = JS("function() {this.yAxis[0].update({type: 'linear'});}")
            ),
            customButton2 = list(
              text = "Log",
              onclick = JS("function() {this.yAxis[0].update({type: 'logarithmic'});}")
            )
          )
        ) %>%
        hc_xAxis(title = list(text = "Time (Days)")) %>%
        hc_yAxis(title = list(text = "Concentration")) %>%
        hc_title(text = "Time Series Plot") %>% 
        hc_size(height= 500) 
    })
  })
})
