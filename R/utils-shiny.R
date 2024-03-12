
# Utils Shiny ----

#' @importFrom htmltools htmlDependency
#' @importFrom utils packageVersion
html_dependency_esquisse <- function() {
  htmlDependency(
    name = "esquisse",
    version = packageVersion("esquisse"),
    package = "esquisse",
    src = c(file = "assets/esquisse"),
    script = c("js/shiny-utils.js"),
    stylesheet = c("css/styles.css", "css/annie-use-your-telescope.css"),
    all_files = TRUE
  )
}

html_dependency_clipboard <- function() {
  htmlDependency(
    name = "clipboard",
    version = "2.0.6",
    package = "esquisse",
    src = c(file = "assets/clipboard"),
    script = c("clipboard.min.js"),
    all_files = FALSE
  )
}

html_dependency_moveable <- function() {
  htmlDependency(
    name = "moveable",
    version = "0.23.0",
    package = "esquisse",
    src = c(file = "assets/moveable"),
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
    message = list(id = session$ns(id), display = display)
  )
}




#' Enable or disable a button
#'
#' @param inputId Input's id to enable / disable.
#' @param type 'enable' or 'disable'.
#' @param session shiny session.
#'
#' @noRd
toggleBtn <- function(inputId,
                      type = "disable",
                      session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "togglewidget",
    message = list(inputId = inputId, type = type)
  )
}




#' @importFrom shinyWidgets prettyToggle
#' @importFrom htmltools css
play_pause_input <- function(inputId, show = TRUE) {
  play_pause <- prettyToggle(
    inputId = inputId,
    value = TRUE,
    label_on = "Play",
    label_off = "Pause",
    outline = TRUE,
    plain = TRUE,
    bigger = TRUE,
    inline = TRUE,
    icon_on = phosphoricons::ph_i("play-circle"),
    icon_off = phosphoricons::ph_i("pause-circle")
  )
  play_pause$attribs$style <- "display: inline-block; margin-right: -5px;"
  tags$div(
    style = css(
      position = "absolute",
      right = "40px",
      top = "5px",
      fontWeight = "bold",
      zIndex = 1000,
      display = if (!isTRUE(show)) "none"
    ),
    class = "esquisse-playpause-btn",
    play_pause
  )
}



button_close_modal <- function() {
  tags$button(
    phosphoricons::ph("x", title = i18n("Close"), height = "2em"),
    class = "btn btn-link",
    style = css(border = "0 none", position = "absolute", top = "5px", right = "5px"),
    `data-dismiss` = "modal",
    `data-bs-dismiss` = "modal",
    `aria-label` = i18n("Close")
  )
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
                   with_moveable = TRUE,
                   session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    if (isTRUE(with_moveable)) paste0("resize-", id) else "esquisse-resize-plot",
    list(
      id = id,
      width = width,
      height = height
    )
  )
}
