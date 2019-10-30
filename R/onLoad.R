#' Adds the content of assets/ to esquisse/
#'
#' @importFrom shiny addResourcePath registerInputHandler
#'
#' @noRd
.onLoad <- function(...) {
  shiny::addResourcePath("esquisse", system.file("assets", package = "esquisse"))
  shiny::registerInputHandler("esquisse.dragula", function(data, ...) {
    if (is.null(data)) {
      NULL
    } else {
      data$source <- unlist(data$source)
      data$target <- lapply(data$target, unlist, recursive = FALSE)
      data
    }
  }, force = TRUE)
  esquisse.palettes <- getOption("esquisse.palettes")
  if (is.null(esquisse.palettes)) {
    options("esquisse.palettes" = default_pals)
  }
  esquisse.themes <- getOption("esquisse.themes")
  if (is.null(esquisse.themes)) {
    options("esquisse.themes" = default_themes)
  }
  esquisse.colors <- getOption("esquisse.colors")
  if (is.null(esquisse.colors)) {
    options("esquisse.colors" = default_cols)
  }
}
