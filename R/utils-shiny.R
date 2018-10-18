
# Utils Shiny ----

#' @importFrom htmltools singleton tags
useShinyUtils <- function() {
  singleton(tags$head(tags$script(src = "esquisse/shiny-utils.js")))
}


#'  Toggle Input Server
#'
#' @param session shiny session.
#' @param inputId shiny input id.
#' @param enable enable enable or disable the input.
#'
#' @noRd
toggleInput <- function(session, inputId, enable = TRUE) {
  session$sendCustomMessage(
    type = 'toggleInput',
    message = list(id = inputId, enable = enable)
  )
}



escape_jquery <- function(string) {
  gsub(x = string, pattern = "(\\W)", replacement = "\\\\\\1")
}




#'  Toggle Input Server
#'
#' @param session shiny session.
#' @param id shiny input id.
#' @param display character, 'none' to hide, 'block' or 'inline-block' to show
#'
#' @noRd
toggleDisplay <- function(session, id, display = c("none", "block", "inline-block")) {
  display <- match.arg(display)
  session$sendCustomMessage(
    type = 'toggleDisplay',
    message = list(id = id, display = display)
  )
}





#' Enable / Disable a Button
#'
#' @param session shiny session.
#' @param inputId Input's id to enable / disable.
#' @param type 'enable' or 'disable'.
#'
#' @noRd
toggleBtn <- function(session, inputId, type = "disable") {
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

