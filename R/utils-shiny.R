
# Utils Shiny ----

#' @importFrom htmltools htmlDependency
#' @importFrom utils packageVersion
html_dependency_esquisse <- function() {
  htmlDependency(
    name = "esquisse",
    version = packageVersion("esquisse"),
    src = c(file = "assets/esquisse", href = "esquisse/esquisse"),
    package = "esquisse",
    script = c("js/shiny-utils.js"),
    stylesheet = c("css/styles.css", "css/annie-use-your-telescope.css"),
    all_files = TRUE
  )
}



#' Enable or disable a Shiny input
#'
#' @param inputId shiny input id.
#' @param enable enable enable or disable the input.
#' @param session shiny session.
#'
#' @noRd
toggleInput <- function(inputId,
                        enable = TRUE,
                        session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "toggleInput",
    message = list(id = inputId, enable = enable)
  )
}



#' Display or hide a Shiny input
#'
#' @param id shiny input id.
#' @param display character, 'none' to hide, 'block' or 'inline-block' to show
#' @param session shiny session.
#'
#' @noRd
toggleDisplay <- function(id,
                          display = c("none", "block", "inline-block"),
                          session = shiny::getDefaultReactiveDomain()) {
  if (is.logical(display)) {
    display <- ifelse(display, "block", "none")
  }
  session$sendCustomMessage(
    type = "toggleDisplay",
    message = list(id = id, display = display)
  )
}




#' Enable or disable a button
#'
#' @param inputId Input's id to enable / disable.
#' @param type 'enable' or 'disable'.
#' @param session shiny session.
#'
#' @noRd
toggleBtn <- function(inputId, type = "disable",
                      session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "togglewidget",
    message = list(inputId = inputId, type = type)
  )
}



#' Tag to display code
#'
#' @param ... Character strings
#'
#' @noRd
rCodeContainer <- function(...) {
  code <- htmltools::HTML(as.character(tags$code(class = "language-r", ...)))
  htmltools::tags$div(htmltools::tags$pre(code))
}

