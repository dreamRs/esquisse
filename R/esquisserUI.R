#' @title Esquisse Shiny module
#' 
#' @description Launch \code{esquisse} in a classic Shiny app.
#'
#' @param id Module's id.
#' @param header Logical. Display or not \code{esquisse} header.
#' @param choose_data Logical. Display or not the button to choose data.
#' 
#' @return A \code{reactiveValues} with 3 slots :
#'  \itemize{
#'   \item \strong{code_plot} : code to generate plot.
#'   \item \strong{code_filters} : a list of length two with code to reproduce filters.
#'   \item \strong{data} : \code{data.frame} used in plot (with filters applied).
#'  }
#' 
#' @note For the module to display correctly, it is necessary to place it in a container with a fixed height.
#'
#' @export
#' 
#' @name module-esquisse
#'
#' @importFrom htmltools tags tagList singleton
#' @importFrom shiny plotOutput icon actionButton NS
#' @importFrom miniUI miniTitleBarButton miniPage
#' @importFrom shinyWidgets prettyToggle
#'
#' @examples 
#' 
#' if (interactive()) {
#' 
#' 
#' ### Part of a Shiny app ###
#' 
#' library(shiny)
#' library(esquisse)
#' 
#' ui <- fluidPage(
#'   tags$h1("Use esquisse as a Shiny module"),
#'   
#'   # Force scroll bar to appear (otherwise hidden by esquisse)
#'   tags$style("html, body {overflow: visible !important;"),
#'   
#'   radioButtons(
#'     inputId = "data", 
#'     label = "Data to use:", 
#'     choices = c("iris", "mtcars"),
#'     inline = TRUE
#'   ),
#'   tags$div(
#'     style = "height: 700px;", # needs to be in fixed height container
#'     esquisserUI(
#'       id = "esquisse", 
#'       header = FALSE, # dont display gadget title
#'       choose_data = FALSE # dont display button to change data
#'     )
#'   )
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   data_r <- reactiveValues(data = iris, name = "iris")
#'   
#'   observeEvent(input$data, {
#'     if (input$data == "iris") {
#'       data_r$data <- iris
#'       data_r$name <- "iris"
#'     } else {
#'       data_r$data <- mtcars
#'       data_r$name <- "mtcars"
#'     }
#'   })
#'   
#'   callModule(module = esquisserServer, id = "esquisse", data = data_r)
#'   
#' }
#' 
#' shinyApp(ui, server)
#' 
#' 
#' 
#' ### Whole Shiny app ###
#' 
#' library(shiny)
#' library(esquisse)
#' 
#' 
#' # Load some datasets in app environment
#' my_data <- data.frame(
#'   var1 = rnorm(100),
#'   var2 = sample(letters[1:5], 100, TRUE)
#' )
#' 
#' 
#' 
#' ui <- fluidPage(
#'   tags$div( # needs to be in fixed height container
#'     style = "position: fixed; top: 0; bottom: 0; right: 0; left: 0;", 
#'     esquisserUI(id = "esquisse")
#'   )
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   callModule(module = esquisserServer, id = "esquisse")
#'   
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
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
      tags$script(src = "esquisse/clipboard/clipboard.min.js")
    )),

    if (isTRUE(header)) box_title,
    # page
    layoutAddin(
      top_left = htmltools::tagList(
        htmltools::tags$div(
          style = "padding: 10px;",
          dropInput(
            inputId = ns("geom"),
            choicesNames = geomIcons()$names, 
            choicesValues = geomIcons()$values,
            dropWidth = "290px",
            width = "100%"
          ),
          if (isTRUE(choose_data)) chooseDataUI(id = ns("choose-data"))
        )
      ),
      top_right = dragulaInput(
        inputId = ns("dragvars"), 
        sourceLabel = "Variables", 
        targetsLabels = c("X", "Y", "Fill", "Color", "Size", "Group", "Facet"), 
        targetsIds = c("xvar", "yvar", "fill", "color", "size", "group", "facet"),
        choices = "",
        badge = FALSE, 
        width = "100%", 
        height = "100%",
        replace = TRUE
      ),
      main = htmltools::tags$div(
        style = "margin-top: 10px; padding-bottom: 25px; height: 100%;",
        tags$div(
          style = "position: absolute; right: 0; top: 10px; font-weight: bold; z-index: 1000;",
          prettyToggle(
            inputId = ns("play_plot"), 
            value = TRUE,
            label_on = "Play",
            label_off = "Pause",
            outline = TRUE,
            plain = TRUE,
            bigger = TRUE, 
            inline = TRUE,
            icon_on = icon("play-circle-o", class = "fa-2x"),
            icon_off = icon("pause-circle-o", class = "fa-2x")
          )
        ),
        shiny::plotOutput(outputId = ns("plooooooot"), width = "100%", height = "100%")
      )
    ),

    chartControlsUI(id = ns("controls"))
  )

}
