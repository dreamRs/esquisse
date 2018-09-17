#' Ui for addin esquisser
#'
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @noRd
#'
#' @importFrom htmltools tags tagList
#' @importFrom shiny plotOutput icon actionButton
#' @importFrom miniUI miniTitleBarButton miniPage
#'
esquisserUI <- function() {

  ### addin
  miniPage(

    # style sheet
    tags$link(rel="stylesheet", type="text/css", href="esquisse/styles.css"),
    tags$script(src = "esquisse/clipboard.min.js"),

    # title
    tags$div(
      class="gadget-title dreamrs-title-box",
      tags$h1(shiny::icon("wrench"), "ggplot2 builder", class = "dreamrs-title"),
      tags$div(
        class = "pull-right",
        miniTitleBarButton(inputId = "close", label = "Close")
      )
    ),
    # page
    layoutAddin(
      top_left = htmltools::tagList(
        htmltools::tags$div(
          style = "padding: 10px;",
          imageButtonUI(
            id = "geom",
            imgs = geom_icon_input(), 
            width = "240px"
          ),
          htmltools::tags$br(),
          chooseDataUI(id = "choose-data")
        )
      ),
      top_right = dragulaInput(
        inputId = "dragvars", sourceLabel = "Variables", 
        targetsLabels = c("X", "Y", "Fill", "Color", "Size"), 
        targetsIds = c("xvar", "yvar", "fill", "color", "size"),
        choices = "", badge = FALSE, width = "100%", height = "100%",
        replace = TRUE
      ),
      main = htmltools::tags$div(
        style = "margin-top: 10px; padding-bottom: 25px; height: 100%;",
        shiny::plotOutput(outputId = "plooooooot", width = "100%", height = "100%")
      )
    ),

    chartControlsUI(id = "controls")
  )

}
