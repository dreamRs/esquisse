#' An addin to easily create chart with ggplot2
#'
#' @param data a data.frame, you can pass a data.frame explicitly to the function, 
#' otherwise you'll have to choose one in your environment.
#'
#' @return code to reproduce chart
#' @export
#'
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom shiny dialogViewer browserViewer runGadget paneViewer
#'
#' @examples
#' \dontrun{
#' esquisser(iris)
#' }
#'
#'
esquisser <- function(data = NULL) {


  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  defaultData <- context$selection[[1]]$text

  # Validate selection
  if (is.null(data) && defaultData %in% ls(pos = globalenv())) {
    defaultDataValid <- get(x = defaultData, envir = globalenv())
    if (!inherits(x = defaultDataValid, what = "data.frame")) {
      data <- NULL
    } else {
      data <- as.data.frame(defaultData)
    }
  }

  # options("charter.ggbuilder.data" = data)
  esquisse.env$data <- data

  display <- getOption("charter.display.mode", default = "dialog")

  if (display == "browser") {
    inviewer <- shiny::browserViewer(browser = getOption("browser"))
  } else if (display == "pane") {
    inviewer <- shiny::paneViewer(minHeight = "maximize")
  } else {
    inviewer <- shiny::dialogViewer(
      "Explore your data with ggplot2",
      width = 1000, height = 750
    )
  }

  shiny::runGadget(app = esquisserUI(), server = esquisserServer, viewer = inviewer)
}


