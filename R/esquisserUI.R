
#' @title Esquisse Shiny module
#' 
#' @description Launch \code{esquisse} in a classic Shiny app.
#'
#' @param id Module's id.
#' @param header Logical. Display or not \code{esquisse} header.
#' @param container Container in which display the addin, 
#'  default is to use \code{esquisseContainer}, see examples.
#'  Use \code{NULL} for no container (behavior in versions <= 0.2.1).
#'  Must be a \code{function}.
#' @param choose_data Logical. Display or not the button to choose data.
#' @param insert_code Logical, Display or not a button to insert the ggplot
#'  code in the current user script (work only in RStudio).
#' 
#' @return A \code{reactiveValues} with 3 slots :
#'  \itemize{
#'   \item \strong{code_plot} : code to generate plot.
#'   \item \strong{code_filters} : a list of length two with code to reproduce filters.
#'   \item \strong{data} : \code{data.frame} used in plot (with filters applied).
#'  }
#' 
#' @note For the module to display correctly, it is necessary to place
#'  it in a container with a fixed height. Since version >= 0.2.2, the 
#'  container is added by default.
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
#' if (interactive()) {
#' 
#' ### Part of a Shiny app ###
#' 
#' library(shiny)
#' library(esquisse)
#' 
#' ui <- fluidPage(
#'   tags$h1("Use esquisse as a Shiny module"),
#'   
#'   radioButtons(
#'     inputId = "data", 
#'     label = "Data to use:", 
#'     choices = c("iris", "mtcars"),
#'     inline = TRUE
#'   ),
#'   esquisserUI(
#'     id = "esquisse", 
#'     header = FALSE, # dont display gadget title
#'     choose_data = FALSE, # dont display button to change data,
#'     container = esquisseContainer(height = "700px")
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
#' ui <- fluidPage(
#'   esquisserUI(
#'     id = "esquisse", 
#'     container = esquisseContainer(fixed = TRUE)
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
#' 
#' 
#' ## You can also use a vector of margins for the fixed argument,
#' # useful if you have a navbar for example
#' 
#' ui <- navbarPage(
#'   title = "My navbar app",
#'   tabPanel(
#'     title = "esquisse",
#'     esquisserUI(
#'       id = "esquisse", 
#'       header = FALSE,
#'       container = esquisseContainer(
#'         fixed = c(50, 0, 0, 0)
#'       )
#'     )
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
esquisserUI <- function(id, header = TRUE,
                        container = esquisseContainer(),
                        choose_data = TRUE, 
                        insert_code = FALSE) {
  
  ns <- NS(id)
  
  box_title <- tags$div(
    class="gadget-title dreamrs-title-box",
    tags$h1(shiny::icon("wrench"), "ggplot2 builder", class = "dreamrs-title"),
    tags$div(
      class = "pull-right",
      miniTitleBarButton(inputId = ns("close"), label = "Close")
    ),
    if (isTRUE(choose_data) & isTRUE(header)) {
      tags$div(
        class = "pull-left",
        chooseDataUI(id = ns("choose-data"), class = "btn-sm")
      )
    }
  )
    
  addin <- miniPage(

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
          style = if (isTRUE(choose_data) & !isTRUE(header)) "padding: 10px;" else "padding: 8px; height: 108%;",
          dropInput(
            inputId = ns("geom"),
            choicesNames = geomIcons()$names, 
            choicesValues = geomIcons()$values,
            dropWidth = "290px",
            width = "100%"
          ),
          if (isTRUE(choose_data) & !isTRUE(header)) chooseDataUI(id = ns("choose-data"))
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

    chartControlsUI(id = ns("controls"), insert_code = insert_code)
  )

  if (is.function(container)) {
    addin <- container(addin)
  }
  return(addin)
}

#' @param width,height The width and height of the container, e.g. \code{'400px'},
#'  or \code{'100\%'}; see \code{\link[htmltools]{validateCssUnit}}.
#' @param fixed Use a fixed container, e.g. to use use esquisse full page.
#'  If \code{TRUE}, width and height are ignored. Default to \code{FALSE}.
#'  It's possible to use a vector of CSS unit of length 4 to specify the margins 
#'  (top, right, bottom, left).
#' 
#' @rdname module-esquisse
#' @export
esquisseContainer <- function(width = "100%", height = "700px", fixed = FALSE) {
  function(...) {
    if (identical(fixed, FALSE)) {
      tag <- tags$div(
        style = sprintf("width: %s;", validateCssUnit(width)),
        style = sprintf("height: %s;", validateCssUnit(height)),
        ...
      )
    } else {
      if (identical(fixed, TRUE)) {
        tag <- tags$div(
          style = "position: fixed; top: 0; bottom: 0; right: 0; left: 0;",
          ...
        )
      } else if (length(fixed) == 4) {
        tag <- tags$div(
          style = do.call(
            sprintf,
            c(list(
              fmt = "position: fixed; top: %s; right: %s; bottom: %s; left: %s;"
            ), lapply(fixed, validateCssUnit))
          ),
          ...
        )
      } else {
        stop(
          "fixed must be ever a logical TRUE/FALSE or a vector of length 4 of valid CSS unit.", 
          call. = FALSE
        )
      }
    }
    tagList(
      singleton(tags$head(
        tags$style("html, body {overflow: visible !important;")
      )), tag
    )
  }
}

