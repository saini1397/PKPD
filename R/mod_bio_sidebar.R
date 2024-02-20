#' R Module for the Biodiversity Sidebar Panel.
#'
#' @description
#' R Module for the Biodiversity Sidebar Panel .
#'
#' @import shiny
#' @rdname mod_bio_sidebar

mod_bio_sidebar_ui <- function(id) {
  ns <- NS(id)
  div(
    width = 300,
    div(class = "inlay", style = "height:15px;width:100%;background-color:#ecf0f5"),
    div(id = ns("sidebar"), sidebarMenu(
      div(
        id = "sidebar_button",
        bsButton(
          inputId = ns("reset"),
          label = "Reset",
          icon = icon("home"),
          style = "danger"
        )
      ),
      div(class = "inlay", style = "height:15px;width:100%;background-color:#ecf0f5"),
      menuItem(
        "License",
        tabName = "License",
        icon = icon("tencent-weibo"),
        div(
          checkboxGroupInput(
            inputId = ns("s2"), label = "",
            choices = c("NULL"),
            selected = c("NULL")
          )
        )
      ),
      menuItem(
        "Scientific Name",
        tabName = "Scientific Name",
        icon = icon("filter"),
        div(
          selectInput(
            inputId = ns("s3"), label = "",
            choices = c("NULL"),
            selected = c("NULL"),
            multiple = TRUE
          )
        )
      ),
      menuItem(
        "Basis of Record",
        tabName = "Basis of Record",
        icon = icon("discord"),
        div(
          selectInput(
            inputId = ns("s4"), label = "",
            choices = c("NULL"),
            selected = c("NULL"),
            multiple = TRUE
          )
        )
      )
    ))
  )
}
