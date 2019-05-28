
#' Run module example
#'
#' @param module Module for which to see a demo.
#'
#' @export
#' 
#' @importFrom shiny shinyAppDir
#'
#' @examples
#' 
#' if (interactive()) {
#' 
#' # Demo for filterDF module
#' run_module("filterDF")
#' 
#' }
#' 
run_module <- function(module = c("filterDF", "chooseData", "chooseData2", "coerce")) {
  module <- match.arg(module)
  path <- file.path("modules-examples", module)
  shiny::shinyAppDir(
    appDir = system.file(path, package = "esquisse", mustWork=TRUE), 
    options = list(display.mode = "showcase")
  )
}

