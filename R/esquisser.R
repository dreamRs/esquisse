
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
                      controls = c("options", "labs", "axes", "geoms", "theme", "filters", "code"),
                      viewer = getOption(x = "esquisse.viewer", default = "dialog")) {
  viewer <- match.arg(viewer, choices = c("dialog", "pane", "browser"))

  if (!rstudioapi::isAvailable("1.2")) {
    warning("Esquisse may not work properly, try updating RStudio.", call. = FALSE)
  }

  res_data <- get_data(data, name = deparse(substitute(data)))
  if (!is.null(res_data$esquisse_data)) {
    res_data$esquisse_data <- dropListColumns(res_data$esquisse_data)
  }

  if (viewer == "browser") {
    inviewer <- browserViewer(browser = getOption("browser"))
  } else if (viewer == "pane") {
    inviewer <- paneViewer(minHeight = "maximize")
  } else {
    inviewer <- dialogViewer(
      paste(
        "Les \u00e9toiles sont \u00e9clair\u00e9es pour que chacun puisse un jour retrouver la sienne."
      ),
      width = 1200,
      height = 800
    )
  }

  runGadget(
    app = esquisse_ui(
      id = "esquisse",
      container = function(...) {
        shiny::fillPage(
          theme = bs_theme_esquisse(),
          ...
        )
      },
      insert_code = TRUE,
      controls = controls
    ),
    server = function(input, output, session) {
      esquisse_server(
        id = "esquisse",
        data_rv = res_data$esquisse_data,
        name = res_data$esquisse_data_name,
        import_from = c("env", "file", "copypaste", "googlesheets", "url")
      )
    },
    viewer = inviewer
  )
}



#' Bootstrap Theme for Esquisse
#'
#' @return A [bslib::bs_theme()].
#' @export
#'
#' @importFrom bslib bs_theme bs_add_rules
bs_theme_esquisse <- function() {
  theme <- bslib::bs_theme(
    version = 5L,
    primary = "#112446",
    secondary = "#cccccc",
    preset = "bootstrap",
    font_scale = 0.8,
    "accordion-body-padding-y" = "5px",
    "accordion-body-padding-x" = "5px"
  )
  theme <- bslib::bs_add_rules(
    theme = theme,
    c(
      ".modal-title { @extend .mt-0 }",
      ".sidebar-title { @extend .mb-0 }",
      "#NotiflixNotifyWrap { inset: auto 5px 38px auto !important; }",
      ".nav-tabs .nav-link.active { @extend .border-0; @extend .border-bottom; @extend .border-primary; @extend .border-2;}",
      ".nav-tabs .nav-link.active { @extend .text-primary; @extend .fw-bold}",
      ".esquisse-geom-aes-main .nav-pills .nav-link { @extend .py-1 }"
    )
  )
  return(theme)
}

