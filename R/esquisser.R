
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
                      coerceVars = getOption(x = "esquisse.coerceVars", default = TRUE),
                      viewer = getOption(x = "esquisse.viewer", default = "dialog")) {
  
  options("esquisse.coerceVars" = coerceVars)

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
      "C'est le temps que tu as perdu pour ta rose qui rend ta rose importante.",
      width = 1000, height = 750
    )
  }

  runGadget(
    app = esquisserUI(id = "esquisse", container = NULL, insert_code = TRUE), 
    server = function(input, output, session) {
      callModule(
        module = esquisserServer, 
        id = "esquisse", 
        data = rv
      )
    }, 
    viewer = inviewer
  )
}


