
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

html_dependency_clipboard <- function() {
  htmlDependency(
    name = "clipboard",
    version = "2.0.6",
    src = c(file = "assets/clipboard", href = "esquisse/clipboard"),
    script = c("clipboard.min.js"),
    all_files = FALSE
  )
}

html_dependency_moveable <- function() {
  htmlDependency(
    name = "moveable",
    version = "0.23.0",
    src = c(file = "assets/moveable", href = "esquisse/moveable"),
    script = c("moveable.min.js", "resizer-handler.js"),
    all_files = FALSE
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


# Resizer handlers

activate_resizer <- function(id,
                             ..., 
                             modal = FALSE,
                             container = "body", 
                             session = shiny::getDefaultReactiveDomain()) {
  if (isTRUE(modal))
    container <- ".modal-body"
  session$sendCustomMessage("resize", list(
    id = id,
    container = container,
    ...,
    modal = modal
  ))
}

resize <- function(id, 
                   width, 
                   height, 
                   session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(paste0("resize-", id), list(
    width = width,
    height = height
  ))
}
