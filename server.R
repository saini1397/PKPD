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


  timepoint <- reactive({
    if (input$s1 == "adpc") {
      "ATPT"
    } else if (input$s1 == "adpp") {
      "APERIODC"
    }
  })


  colvars <- reactive({
    if (length(names(x1())[names(x1()) %in% c("PARAMCD")]) != 0) {
      c("STUDYID", "SUBJID", "PCTEST", "PARAMCD", "AVAL")
    } else {
      names(x1())[1:10]
    }
  })

 
  ################## Listing Purpose
  # To update drop down dynamically 
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
      expre <- parse(text = input$text1)

      if (input$text1 != "") {
        x1() %>%
          filter(eval(expre))
      } else {
        {
          x1()
        } %>%
          select(one_of(input$colselect)) %>%
          datatable(filter = "top", extensions = "Buttons", options = list(
            scrollX = TRUE,
            pageLength = 25, autoWidth = FALSE,
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel", "pdf")
          ))
      }
    }) %>% bindEvent(input$saveFilterButton1)
  })


  observe({
    updateSelectInput(session, "param1",
      choices = c(names(x1())),
      selected = "PCTEST"
    )

    updateSelectInput(session, "param2",
      choices = unique(x1()$PCTEST),
      selected = "ANAL1"
    )

    updateSelectInput(session, "subj2",
      choices = unique(x1()$SUBJID),
      selected = "1001015"
    )

    updateSelectInput(session, "visit1",
      choices = c(names(x1())),
      selected = "VISITNUM"
    )

    updateSelectInput(session, "time1",
      choices = c(names(x1())),
      selected = timepoint()
    )


    updateSelectInput(session, "visit2",
      choices = c(names(x1())),
      selected = "VISITNUM"
    )

    updateTextInput(
      session = getDefaultReactiveDomain(),
      inputId = "text3",
      label = NULL,
      value = NULL,
      placeholder = NULL
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



  # Figure part

  fig <- reactive({
    req(x1())
    x1() %>%
      filter(PCTEST %in% (input$param2) & SUBJID %in% (input$subj2)) %>%
      mutate(ARRLT = (ADTM - TRTSDTM) / 3600) %>%
      mutate(ARRLT = round(ARRLT, digits = 2)) %>%
      mutate(ARRLT = if_else(ARRLT < 0, 0, ARRLT)) %>%
      arrange(PCTESTCD, SUBJID, ARRLT, AVAL)
  })

  observeEvent(session, {
    output$chart2 <- renderHighchart({
      fig() %>%
        mutate(ARRLT = (ADTM - TRTSDTM) / 3600) %>%
        mutate(ARRLT = round(ARRLT, digits = 2)) %>%
        mutate(ARRLT = if_else(ARRLT < 0, 0, ARRLT)) %>%
        arrange(PCTESTCD, SUBJID, ARRLT, AVAL) %>%
        hchart(
          "line", hcaes(x = ARRLT, y = AVAL, group = c(SUBJID))
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
        hc_xAxis(title = list(text = "Time (Hrs)")) %>%
        hc_yAxis(title = list(text = "Concentration (ng/mL)")) %>%
        hc_title(text = "Time Series Plot") %>%
        hc_size(height = 750) %>%
        hc_tooltip(formatter = JS("function(){return '<b>Subject: ' + this.series.name + '</b><br>Timepoint: ' + this.x + ' Hrs' + '<br>Conc: ' + this.y+ ' ng/mL';}"))
    })
  })
})
