
# Utils Shiny ----


#' Toggle Input UI
#'
#' @return a script tag.
#' @noRd
toggleInputUi <- function() {
  htmltools::tags$script(
    paste(
      "Shiny.addCustomMessageHandler('toggleInput',",
      "function(data) {",
      "$('#' + data.id).prop('disabled', !data.enable);",
      "if (data.picker) {",
      "$('#' + data.id).selectpicker('refresh');",
      "}",
      "}",
      ");",
      sep = "\n"
    )
  )
}
#'  Toggle Input Server
#'
#' @param session shiny session.
#' @param inputId shiny input id.
#' @param enable enable enable or disable the input.
#' @param picker Is the input a \code{pickerInput}.
#'
#' @noRd
toggleInputServer <- function(session, inputId, enable = TRUE, picker = FALSE) {
  session$sendCustomMessage(
    type = 'toggleInput',
    message = list(id = escape_jquery(inputId), enable = enable, picker = picker)
  )
}



escape_jquery <- function(string) {
  gsub(x = string, pattern = "(\\W)", replacement = "\\\\\\1")
}





#' Hide/Show an HTML tag
#'
#' @return a script tag.
#' @noRd
toggleDisplayUi <- function() {
  htmltools::tags$script(
    paste(
      "Shiny.addCustomMessageHandler('toggleDisplay',",
      "function(data) {",
      "$('#' + data.id).css('display', data.display);",
      "});",
      sep = "\n"
    )
  )
}
#'  Toggle Input Server
#'
#' @param session shiny session.
#' @param id shiny input id.
#' @param display character, 'none' to hide, 'block' or 'inline-block' to show
#'
#' @noRd
toggleDisplayServer <- function(session, id, display = c("none", "block", "inline-block")) {
  display <- match.arg(display)
  session$sendCustomMessage(
    type = 'toggleDisplay',
    message = list(id = escape_jquery(id), display = display)
  )
}






#' Enable / Disable a Button
#'
#' @details This function must be used in the ui loaded on the launch of the application
#'
#' @return a script tag
#' @noRd
toggleBtnUi <- function() {
  js <- paste(
    "Shiny.addCustomMessageHandler('togglewidget', function(data) {",
    "if (data.type == 'disable') {",
    "$('#' + data.inputId).prop('disabled', true);",
    "$('#' + data.inputId).addClass('disabled');",
    "}",
    "if (data.type == 'enable') {",
    "$('#' + data.inputId).prop('disabled', false);",
    "$('#' + data.inputId).removeClass('disabled');",
    "}",
    "});", collapse = "\n"
  )
  htmltools::tags$script(js)
}

#' Enable / Disable a Button
#'
#' @param message A message id (the same as in ui)
#' @param inputId Input's id to enable / disable
#' @param type 'enable' or 'disable'
#'
#' @return nothing
#' @noRd
toggleBtnServer <- function(session, inputId, type = "disable") {
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

