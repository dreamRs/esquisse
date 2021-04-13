
# Avance sur ta route, car elle n'existe que par ta marche.
# S-A


#' @title An add-in to easily create plots with ggplot2
#' 
#' @description Select data to be used and map variables to aesthetics to produce a chart,
#'  customize common elements and get code to reproduce the chart.
#'
#' @param data a `data.frame`, you can pass a `data.frame` explicitly to the function,
#'  otherwise you'll have to choose one in global environment.
#' @param controls Controls menu to be displayed. Use `NULL` to hide all menus.
#' @param viewer Where to display the gadget: `"dialog"`,
#'  `"pane"` or `"browser"` (see \code{\link[shiny]{viewer}}).
#'
#' @return `NULL`. You can view code used to produce the chart, copy it or insert it in current script.
#' @export
#'
#' @importFrom shiny dialogViewer browserViewer runGadget paneViewer reactiveValues
#'
#' @examples
#' if (interactive()) {
#' # Launch with :
#' esquisser(iris)
#' # If in RStudio it will be launched by default in dialog window
#' # If not, it will be launched in browser
#'
#' # Launch esquisse in browser :
#' esquisser(iris, viewer = "browser")
#'
#' # You can set this option in .Rprofile :
#' options("esquisse.viewer" = "viewer")
#' # or
#' options("esquisse.viewer" = "browser")
#'
#' # esquisse use shiny::runApp
#' # see ?shiny::runApp to see options
#' # available, example to use custom port:
#'
#' options("shiny.port" = 8080)
#' esquisser(iris, viewer = "browser")
#'
#' }
esquisser <- function(data = NULL,
                      controls = c("labs", "parameters", "appearance", "filters", "code"),
                      viewer = getOption(x = "esquisse.viewer", default = "dialog")) {
  viewer <- match.arg(viewer, choices = c("dialog", "pane", "browser"))
  
  if (!rstudioapi::isAvailable("1.2")) {
    warning("Esquisse may not work properly, try updating RStudio.", call. = FALSE)
  }

  res_data <- get_data(data, name = deparse(substitute(data)))
  if (!is.null(res_data$esquisse_data)) {
    res_data$esquisse_data <- dropListColumns(res_data$esquisse_data)
  }
  rv <- reactiveValues(
    data = res_data$esquisse_data,
    name = res_data$esquisse_data_name
  )

  if (viewer == "browser") {
    inviewer <- browserViewer(browser = getOption("browser"))
  } else if (viewer == "pane") {
    inviewer <- paneViewer(minHeight = "maximize")
  } else {
    inviewer <- dialogViewer(
      paste(
        "Les grandes personnes ne comprennent jamais rien toutes seules, et c'est fatigant,",
        "pour les enfants, de toujours et toujours leur donner des explications."
      ),
      width = 1100,
      height = 750
    )
  }

  runGadget(
    app = esquisse_ui(
      id = "esquisse",
      container = NULL,
      insert_code = TRUE,
      controls = controls
    ),
    server = function(input, output, session) {
      esquisse_server("esquisse", rv)
    },
    viewer = inviewer
  )
}


