
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
#' @param disable_filters Logical. Disable the menu allowing to filter data used.
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
#' @importFrom shiny fillPage plotOutput icon actionButton NS fluidRow column fillCol
#' @importFrom shinyWidgets prettyToggle
#'
#' @example examples/esquisse-module.R
esquisserUI <- function(id, header = TRUE,
                        container = esquisseContainer(),
                        choose_data = TRUE,
                        insert_code = FALSE,
                        disable_filters = FALSE) {

  ns <- NS(id)

  box_title <- tags$div(
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
    if (isTRUE(choose_data) & isTRUE(header)) {
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
    }
  )

  addin <- fillPage(tags$div(
    class = "esquisse-container",
    html_dependency_esquisse(),
    singleton(x = tagList(
      tags$script(src = "esquisse/clipboard/clipboard.min.js")
    )),

    if (isTRUE(header)) box_title,

    fluidRow(
      class = "row-no-gutters",
      column(
        width = 1,
        tags$div(
          style = if (isTRUE(choose_data) & !isTRUE(header)) {
            "padding: 10px;"
          } else {
            "padding: 3px 3px 0 3px; height: 144px;"
          },
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
      column(
        width = 11,
        uiOutput(outputId = ns("ui_aesthetics"))
      )
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
      disable_filters = disable_filters
    )
  ))

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

