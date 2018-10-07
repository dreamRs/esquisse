#' @title Esquisse Shiny module
#' 
#' @description Launch \code{esquisse} in a classic Shiny app.
#'
#' @param id Module's id.
#' @param header Logical. Display or not \code{esquisse} header.
#' @param choose_data Logical. Display or not the button to choose data.
#'
#' @export
#' 
#' @name esquisse-module
#'
#' @importFrom htmltools tags tagList singleton
#' @importFrom shiny plotOutput icon actionButton NS
#' @importFrom miniUI miniTitleBarButton miniPage
#'
esquisserUI <- function(id, header = TRUE, choose_data = TRUE) {
  
  ns <- NS(id)
  
  
  box_title <- tags$div(
    class="gadget-title dreamrs-title-box",
    tags$h1(shiny::icon("wrench"), "ggplot2 builder", class = "dreamrs-title"),
    tags$div(
      class = "pull-right",
      miniTitleBarButton(inputId = ns("close"), label = "Close")
    )
  )
    

  ### addin
  miniPage(

    # style sheet
    singleton(x = tagList(
      tags$link(rel="stylesheet", type="text/css", href="esquisse/styles.css"),
      tags$script(src = "esquisse/clipboard.min.js")
    )),

    if (isTRUE(header)) box_title,
    # page
    layoutAddin(
      top_left = htmltools::tagList(
        htmltools::tags$div(
          style = "padding: 10px;",
          imageButtonUI(
            id = ns("geom"),
            imgs = geom_icon_input(), 
            width = "240px"
          ),
          # htmltools::tags$br(),
          if (isTRUE(choose_data)) chooseDataUI(id = ns("choose-data"))
        )
      ),
      top_right = dragulaInput(
        inputId = ns("dragvars"), sourceLabel = "Variables", 
        targetsLabels = c("X", "Y", "Fill", "Color", "Size"), 
        targetsIds = c("xvar", "yvar", "fill", "color", "size"),
        choices = "", badge = FALSE, width = "100%", height = "100%",
        replace = TRUE
      ),
      main = htmltools::tags$div(
        style = "margin-top: 10px; padding-bottom: 25px; height: 100%;",
        shiny::plotOutput(outputId = ns("plooooooot"), width = "100%", height = "100%")
      )
    ),

    chartControlsUI(id = ns("controls"))
  )

}
