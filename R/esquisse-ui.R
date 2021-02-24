
#' @title Esquisse module
#' 
#' @description Use esquisse as a module in Shiny applications
#'
#' @param id Modules's ID.
#' @param header Logical. Display or not \code{esquisse} header.
#' @param container Container in which display the addin,
#'  default is to use \code{esquisseContainer}, see examples.
#'  Use \code{NULL} for no container (behavior in versions <= 0.2.1).
#'  Must be a \code{function}.
#' @param controls Controls menu to be displayed. Use \code{NULL} to hide all menus.
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
#'
#' @export
#' 
#' @name esquisse-module
#' @order 1
#'
#' @importFrom htmltools tags tagList singleton
#' @importFrom shiny fillPage plotOutput icon actionButton NS fluidRow column fillCol
#' @importFrom shinyWidgets prettyToggle
#'
#' @example examples/esquisse-module-1.R
#' 
#' @example examples/esquisse-module-2.R
#' 
#' @example examples/esquisse-module-3.R
esquisse_ui <- function(id, 
                        header = TRUE,
                        container = esquisseContainer(),
                        controls = c("labs", "parameters", "appearance", "filters", "code"),
                        insert_code = FALSE) {
  ns <- NS(id)
  tag_header <- tags$div(
    class = "esquisse-title-container",
    tags$h1("Esquisse", class = "esquisse-title"),
    tags$div(
      class = "pull-right",
      actionButton(
        inputId = ns("settings"),
        label = NULL,
        icon = icon("gear", class = "fa-lg"),
        class = "btn-sm",
        title = "Display settings"
      ),
      actionButton(
        inputId = ns("close"),
        label = NULL,
        icon = icon("times", class = "fa-lg"),
        class = "btn-sm",
        title = "Close Window"
      )
    ),
    tags$div(
      class = "pull-left",
      actionButton(
        inputId = ns("launch_import_data"),
        label = NULL,
        icon = icon("database", class = "fa-lg"),
        class = "btn-sm",
        title = "Import data"
      )
    )
  )
  
  ui <- fillPage(tags$div(
    class = "esquisse-container",
    html_dependency_esquisse(),
    singleton(x = tagList(
      tags$script(src = "esquisse/clipboard/clipboard.min.js")
    )),
    shinyWidgets::chooseSliderSkin("Modern", "#112446"),
    
    if (isTRUE(header)) tag_header,
    
    tags$div(
      class = "esquisse-geom-aes",
      tags$div(
        style = "padding: 3px 3px 0 3px; height: 144px;",
        dropInput(
          inputId = ns("geom"),
          choicesNames = geomIcons()$names,
          choicesValues = geomIcons()$values,
          dropWidth = "290px",
          width = "100%"
        )
      ),
      uiOutput(outputId = ns("ui_aesthetics"))
    ),
    
    fillCol(
      style = "overflow-y: auto;",
      tags$div(
        style = "height: 100%; min-height: 400px;",
        tags$div(
          style = "position: absolute; right: 0; top: 5px; font-weight: bold; z-index: 1000;",
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
        plotOutput(outputId = ns("plooooooot"), width = "100%", height = "100%")
      )
    ),
    
    controls_ui(
      id = ns("controls"),
      insert_code = insert_code,
      controls = controls
    )
  ))
  
  if (is.function(container)) {
    ui <- container(ui)
  }
  return(ui)
}

#' @param width,height The width and height of the container, e.g. \code{'400px'},
#'  or \code{'100\%'}; see \code{\link[htmltools]{validateCssUnit}}.
#' @param fixed Use a fixed container, e.g. to use use esquisse full page.
#'  If \code{TRUE}, width and height are ignored. Default to \code{FALSE}.
#'  It's possible to use a vector of CSS unit of length 4 to specify the margins
#'  (top, right, bottom, left).
#'
#' @rdname esquisse-module
#' @order 3
#' 
#' @export
esquisseContainer <- function(width = "100%", height = "700px", fixed = FALSE) {
  force(width)
  force(height)
  force(fixed)
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
