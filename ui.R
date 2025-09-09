ui <- dashboardPage(
  skin = "purple",
  title = "PKPD",
  dashboardHeader(
    title = span("PK/PD"),
    titleWidth = 300,
    tags$li(
      a(
        strong("GitHub Codes !!"),
        height = 40,
        href = "https://github.com/saini1397/PKPD",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
  ),
  dashboardSidebar(
    width = 300,
    div(class = "inlay", style = "height:15px;width:100%;background-color:#ecf0f5"),
    sidebarMenu(
      # div(
      #   id = "sidebar_button",
      #   bsButton(
      #     inputId = "reset",
      #     label = "",
      #     icon = icon("pagelines"),
      #     style = "danger"
      #   )
      # ),
      div(class = "inlay", style = "height:15px;width:100%;background-color:#ecf0f5"),
      menuItem(
        "PK/PD Dataset",
        tabName = "PK/PD Dataset",
        icon = icon("filter"),
        div(
          selectInput(
            inputId = "s1", label = "",
            choices = adam_list,
            selected = c("adpc"),
            multiple = FALSE
          )
        )
      ),
      menuItem(
        "Select Population ",
        tabName = "Select Pipulation",
        icon = icon("soundcloud"),
        div(
          selectInput(
            inputId = "s2", label = "",
            choices = c("PKFL", "SAFFL", "ITTFL", "EFFFL"),
            selected = c("PKFL"),
            multiple = FALSE
          )
        )
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "www/bio.css"
      )
    ),
    div(fluidRow(
      div(
        column(
          width = 12,
          tabBox(
            width = NULL,
            height = 800,
            tabPanel(
              useShinyjs(),
              title = "PK/PD Listing",
              div(
                div(
                  column(
                    width = 4,
                    shinyWidgets::pickerInput(
                      inputId = "colselect", label = "Select Columns:",
                      choices = c(""),
                      multiple = TRUE,
                      selected = NULL,
                      options = shinyWidgets::pickerOptions(
                        actionsBox = TRUE,
                        title = "Select Columns to Display",
                      ), width = "100%"
                    )
                  ),
                  column(width = 3, textInput(
                    "text1",
                    "Subset Data",
                    placeholder = "e.g. PCTEST=='ANAL3' & PCREFID=='102'"
                  )), br(), column(width = 1, actionButton(
                    width = "250px",
                    inputId = "saveFilterButton1",
                    label = "Apply Subset & Generate Listing",
                    icon = icon("area-chart"),
                    size = "md",
                    class = "btn-info"
                  )),
                ), br(), br(), br(),
                div(
                  withSpinner(
                    dataTableOutput("data_table"),
                    type = 4,
                    color = "#2E8B57",
                    size = 0.7
                  )
                )
              )
            ), tabPanel(
              useShinyjs(),
              title = "PK/PD Table",
              div(
                div(column(
                  width = 4,
                  selectInput(
                    inputId = "param1", label = "Select Parameter:",
                    choices = c("AVAL", "AVALC"),
                    multiple = FALSE,
                    selected = "PCTEST"
                  )
                )),
                div(column(
                  width = 4,
                  selectInput(
                    inputId = "visit1", label = "Select Visit:",
                    choices = c("AVAL", "AVALC"),
                    multiple = FALSE,
                    selected = "AVAL"
                  )
                )),
                div(column(
                  width = 4,
                  selectInput(
                    inputId = "time1", label = "Select Timepoint:",
                    choices = c("NULL"),
                    multiple = FALSE,
                    selected = "NULL"
                  )
                )),br(),
                div(
                  withSpinner(
                    dataTableOutput("data_table2"),
                    type = 4,
                    color = "#2E8B57",
                    size = 0.7
                  )
                )
              )
            ),
            tabPanel(
              useShinyjs(),
              title = "PK/PD Figure",
              div(
                div(column(
                  width = 3,
                  selectInput(
                    inputId = "param2", label = "Select Parameter:",
                    choices = c(""),
                    multiple = TRUE
                  )
                )),
                div(column(
                  width = 9,
                  selectInput(
                    inputId = "subj2", label = "Select Subjects:",
                    choices = c(""),
                    multiple = TRUE
                  )
                )),
                div(
                  withSpinner(
                    highchartOutput("chart2",height='750px'),
                    type = 4,
                    color = "#2E8B57",
                    size = 0.7
                  )
                )
              )
            )
          )
        )
      )
    ))
  )
)
