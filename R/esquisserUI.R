
#' @title Esquisse Shiny module
#'
#' @description DEPRECATED, see \code{\link{esquisse-module}}.
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
#'   * \strong{code_plot} : code to generate plot.
#'   * \strong{code_filters} : a list of length two with code to reproduce filters.
#'   * \strong{data} : \code{data.frame} used in plot (with filters applied).
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
esquisserUI <- function(id, header = TRUE,
                        container = esquisseContainer(),
                        choose_data = TRUE,
                        insert_code = FALSE,
                        disable_filters = FALSE) {
  .Deprecated(new = "esquisse_ui", package = "esquisse", old = "esquisserUI")
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
    shinyWidgets::chooseSliderSkin("Modern", "#112446"),

    if (isTRUE(header)) box_title,

    tags$div(
      class = "esquisse-geom-aes",
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
      controls = if (isTRUE(disable_filters)) {
        c("labs", "parameters", "appearance", "code")
      } else {
        c("labs", "parameters", "appearance", "filters", "code")
      }
    )
  ))

  if (is.function(container)) {
    addin <- container(addin)
  }
  return(addin)
}

