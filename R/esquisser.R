
# Avance sur ta route, car elle n'existe que par ta marche.
# S-A


#' An add-in to easily create plots with ggplot2
#'
#' @param data a data.frame, you can pass a data.frame explicitly to the function, 
#' otherwise you'll have to choose one in global environment.
#' @param coerceVars If \code{TRUE} allow to coerce variables to different type when selecting data.
#' @param viewer Where to display the gadget: \code{"dialog"},
#'  \code{"pane"} or \code{"browser"} (see \code{\link[shiny]{viewer}}).
#'
#' @return code to reproduce chart.
#' @export
#'
#' @importFrom shiny dialogViewer browserViewer runGadget paneViewer
#'
#' @examples
#' if (interactive()) {
#' # Launch with:
#' esquisser(iris)
#' # If in RStudio it will be launched by default in dialog window
#' # If not, it will be launched in browser
#' 
#' # change diplay mode with:
#' options("esquisse.display.mode" = "viewer")
#' # or
#' options("esquisse.display.mode" = "browser")
#' }
esquisser <- function(data = NULL, 
                      coerceVars = getOption(x = "esquisse.coerceVars", default = FALSE),
                      viewer = getOption(x = "esquisse.viewer", default = "dialog")) {
  
  options("esquisse.coerceVars" = coerceVars)

  res_data <- get_data(data, name = deparse(substitute(data)))

  esquisse.env$data <- res_data$esquisse_data
  esquisse.env$data_name <- res_data$esquisse_data_name

  if (viewer == "browser") {
    inviewer <- browserViewer(browser = getOption("browser"))
  } else if (viewer == "pane") {
    inviewer <- paneViewer(minHeight = "maximize")
  } else {
    inviewer <- dialogViewer(
      "Explore your data with ggplot2",
      width = 1000, height = 750
    )
  }

  runGadget(app = esquisserUI(), server = esquisserServer, viewer = inviewer)
}


