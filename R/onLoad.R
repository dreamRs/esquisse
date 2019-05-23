#' Adds the content of www to charter/
#'
#' @importFrom shiny addResourcePath registerInputHandler
#'
#' @noRd
.onLoad <- function(...) {
  shiny::addResourcePath("esquisse", system.file('assets', package = "esquisse"))
  shiny::registerInputHandler("esquisse.dragula", function(data, ...) {
    if (is.null(data)) {
      NULL
    } else {
      data$source <- unlist(data$source)
      data$target <- lapply(data$target, unlist, recursive = FALSE)
      data
    }
  }, force = TRUE)
}
