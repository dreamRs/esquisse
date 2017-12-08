#' Adds the content of www to charter/
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
.onLoad <- function(...) {
  shiny::addResourcePath("esquisse", system.file('www', package = "esquisse"))
}
