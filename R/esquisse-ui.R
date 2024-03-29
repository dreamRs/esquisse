
#' @title Esquisse module
#'
#' @description Use esquisse as a module in a Shiny application.
#'
#' @param id Module ID.
#' @param header Either `TRUE` or `FALSE` to display or not `esquisse` header, or a named `list`
#'  where names are : `settings`, `close`, `import` and `show_data` and values are `TRUE` or
#'  `FALSE` to display or not the corresponding button.
#' @param container Container in which display the addin,
#'  default is to use `esquisseContainer`, see examples.
#'  Use `NULL` for no container (behavior in versions <= 0.2.1).
#'  Must be a `function`.
#' @param controls Controls menu to be displayed. Use `NULL` to hide all menus.
#' @param insert_code Logical, Display or not a button to insert the ggplot
#'  code in the current user script (work only in RStudio).
#' @param play_pause Display or not the play / pause button.
#' @param layout_sidebar Put controls in a sidebar on the left rather than below the chart in dropdowns.
#' @param downloads Export options available or `NULL` for no export. See [downloads_labels()].
#'
#' @return A `reactiveValues` with 3 slots :
#'   * **code_plot** : code to generate plot.
#'   * **code_filters** : a list of length two with code to reproduce filters.
#'   * **data** : `data.frame` used in plot (with filters applied).
#'
#'
#' @export
#'
#' @name esquisse-module
#' @order 1
#'
#' @importFrom htmltools tags tagList
#' @importFrom shiny fillPage plotOutput actionButton NS fluidRow column fillCol
#' @importFrom shinyWidgets prettyToggle
#' @importFrom rlang is_list
#' @importFrom utils modifyList
#' @importFrom bslib layout_sidebar sidebar
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
                        insert_code = FALSE,
                        play_pause = TRUE,
                        layout_sidebar = FALSE,
                        downloads = downloads_labels()) {
  ns <- NS(id)
  header_btns <- list(settings = TRUE, close = TRUE, import = TRUE, show_data = TRUE)
  if (is_list(header)) {
    header_btns <- modifyList(header_btns, header)
    header <- TRUE
  }
  tag_header <- tags$div(
    class = "esquisse-title-container",
    tags$h1("Esquisse", class = "esquisse-title"),
    tags$div(
      class = "pull-right float-end",
      if (isTRUE(header_btns$settings)) {
        actionButton(
          inputId = ns("settings"),
          label = ph("gear-six", height = "2em", title = i18n("Display settings")),
          class = "btn-sm",
          title = i18n("Display settings")
        )
      },
      if (isTRUE(header_btns$close)) {
        actionButton(
          inputId = ns("close"),
          label = ph("x", height = "2em", title = i18n("Close Window")),
          class = "btn-sm",
          title = i18n("Close Window")
        )
      }
    ),
    tags$div(
      class = "pull-left",
      if (isTRUE(header_btns$import)) {
        actionButton(
          inputId = ns("launch_import_data"),
          label = ph("database", height = "2em", title = i18n("Import data")),
          class = "btn-sm",
          title = i18n("Import data")
        )
      },
      if (isTRUE(header_btns$show_data)) show_data_ui(ns("show_data"))
    )
  )

  ui <- tags$div(
    class = "esquisse-container",
    html_dependency_esquisse(),
    html_dependency_clipboard(),

    if (isTRUE(header)) tag_header,

    if (!isTRUE(layout_sidebar)) {
      tagList(
        select_geom_aes_ui(ns("geomaes")),

        fillCol(
          style = "overflow-y: auto;",
          tags$div(
            class = "ggplot-output-container",
            play_pause_input(ns("play_plot"), show = play_pause),
            ggplot_output(
              id = ns("plooooooot"),
              width = "100%",
              height = "100%",
              downloads = if ("export" %in% controls) NULL else downloads
            )
          )
        ),

        controls_ui(
          id = ns("controls"),
          insert_code = insert_code,
          controls = controls,
          downloads = downloads
        )
      )
    } else {
      bslib::layout_sidebar(
        padding = 0,
        fillable = FALSE,
        fill = FALSE,
        height = "100%",
        sidebar = bslib::sidebar(
          position = "right",
          # open = "always",
          title = "CONTROLS",
          width = 350,
          controls_ui(
            id = ns("controls"),
            insert_code = insert_code,
            controls = controls,
            layout = "accordion",
            downloads = downloads
          )
        ),
        
        tags$div(
          class = "ggplot-geom-aes-container",
          select_geom_aes_ui(ns("geomaes")),
          tags$div(
            class = "ggplot-output-sidebar-container",
            play_pause_input(ns("play_plot"), show = play_pause),
            ggplot_output(
              id = ns("plooooooot"),
              width = "100%",
              height = "100%",
              downloads = if ("export" %in% controls) NULL else downloads
            )
          )
        )
      )
    }
  )

  if (is.function(container)) {
    ui <- container(ui)
  }
  return(ui)
}

#' @param width,height The width and height of the container, e.g. `"400px"`,
#'  or `"100%"`; see \code{\link[htmltools]{validateCssUnit}}.
#' @param fixed Use a fixed container, e.g. to use use esquisse full page.
#'  If `TRUE`, width and height are ignored. Default to `FALSE`.
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

