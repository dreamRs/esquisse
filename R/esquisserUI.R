#' Ui for addin charter
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
  miniUI::miniPage(

    # style sheet
    htmltools::tags$link(rel="stylesheet", type="text/css", href="esquisse/styles.css"),
    htmltools::tags$script(src = "esquisse/clipboard.min.js"),

    # title
    htmltools::tags$div(
      class="gadget-title dreamrs-title-box",
      htmltools::tags$h1(shiny::icon("wrench"), "ggplot2 builder", class = "dreamrs-title"),
      htmltools::tags$div(
        class = "pull-right",
        miniUI::miniTitleBarButton(inputId = "close", label = "Close")
      )
    ),
    chooseDataUI(),

    # page
    layoutAddin(
      top_left = htmltools::tagList(
        htmltools::tags$div(
          style = "padding: 10px;",
          imageButtonUI(
            id = "geom",
            imgs = list(
              list(inputId = "auto", img = "esquisse/geomIcon/gg-auto.png", label = "Auto"),
              list(inputId = "line", img = "esquisse/geomIcon/gg-line.png", label = "Line"),
              list(inputId = "bar", img = "esquisse/geomIcon/gg-bar.png", label = "Bar"),
              list(inputId = "histogram", img = "esquisse/geomIcon/gg-histo.png", label = "Histogram"),
              list(inputId = "point", img = "esquisse/geomIcon/gg-point.png", label = "point"),
              list(inputId = "boxplot", img = "esquisse/geomIcon/gg-boxplot.png", label = "Boxplot"),
              list(inputId = "density", img = "esquisse/geomIcon/gg-density.png", label = "Density"),
              list(inputId = "tile", img = "esquisse/geomIcon/gg-tile.png", label = "Tile")
            ), width = "240px"
          ),
          htmltools::tags$br(),
          shiny::actionButton(
            inputId = "changeData", label = "Change data",
            icon = shiny::icon("database"), width = "100%"
          )
        )
      ),
      top_right = dragAndDropUi(
        id = "dragvars",
        dragula_css = "esquisse/dragula/dragula.min.css",
        dragula_js = "esquisse/dragula/dragula.min.js",
        bindings = "esquisse/dragula/dragula-bindings-fun.js",
        styles = "esquisse/styles-dad.css"
      ),
      main = htmltools::tags$div(
        style = "margin-top: 10px; padding-bottom: 25px; height: 100%;",
        shiny::plotOutput(outputId = "plooooooot", width = "100%", height = "100%")
      )
      # main = tags$div(
      #   style = "margin: 10px;",
      #   verbatimTextOutput("test")
      # )
    ),

    chartControlsUI(id = "controls")
  )

}
