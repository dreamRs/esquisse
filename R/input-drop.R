
#' @title Dropdown Input
#' 
#' @description A dropdown menu for selecting a value.
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param choicesNames A \code{tagList} of HTML tags to show in the dropdown menu.
#' @param choicesValues Vector corresponding to \code{choicesNames}
#'  for retrieving values server-side.
#' @param selected The initial selected value, must be an element of \code{choicesValues},
#'  default to the first item of \code{choicesValues}.
#' @param dropUp Open the menu above the button rather than below.
#' @param dropWidth Width of the dropdown menu.
#' @param dropMaxHeight Maximal height for the menu.
#' @param dropPreScrollable Force scroll bar to appear in the menu.
#' @param btnClass Class for buttons in dropdown menu, default is \code{"btn-link"},
#'  you can use for example \code{"btn-default"} to display regular buttons.
#' @param width The width of the input.
#'
#' @export
#' 
#' @importFrom htmltools validateCssUnit tags tagList singleton
#' 
#' @seealso \code{\link{updateDropInput}}
#'
#' @examples
#' if (interactive()) {
#'   
#'   library(shiny)
#'   library(esquisse)
#'   
#'   ui <- fluidPage(
#'     tags$h2("Drop Input"),
#'     dropInput(
#'       inputId = "mydrop",
#'       choicesNames = tagList(
#'         list(icon("home"), style = "width: 100px;"), 
#'         list(icon("flash"), style = "width: 100px;"),
#'         list(icon("cogs"), style = "width: 100px;"),
#'         list(icon("fire"), style = "width: 100px;"), 
#'         list(icon("users"), style = "width: 100px;"), 
#'         list(icon("info"), style = "width: 100px;")
#'       ), 
#'       choicesValues = c("home", "flash", "cogs",
#'                         "fire", "users", "info"),
#'       dropWidth = "220px"
#'     ),
#'     verbatimTextOutput(outputId = "res")
#'   )
#'   
#'   server <- function(input, output, session) {
#'     output$res <- renderPrint({
#'       input$mydrop
#'     })
#'   }
#'   
#'   shinyApp(ui, server)
#'   
#' }
dropInput <- function(inputId, choicesNames, choicesValues, selected = NULL,
                      dropUp = FALSE, dropWidth = NULL, dropMaxHeight = NULL,
                      dropPreScrollable = FALSE, btnClass = "btn-link",
                      width = NULL) {
  if (length(choicesValues) != length(choicesNames))
    stop("dropInput: 'choicesNames' and 'choicesValues' must have same length!", call. = FALSE)
  if (is.null(selected))
    selected <- choicesValues[1]
  if (!selected %in% choicesValues)
    stop("dropInput: 'selected' must be an element of 'choicesValues'", call. = FALSE)
  if (!inherits(choicesNames, "list"))
    stop("dropInput: 'choicesNames' must be a list or a tagList", call. = FALSE)
  tagSelected <- choicesNames[[which(choicesValues == selected)]]
  btn <- do.call(
    what = tags$button,
    args = c(tagSelected, list(
      style = if (!is.null(width))
        paste0("width: ", validateCssUnit(width), ";"),
      class = "btn btn-default drop-input-main dropdown-toggle", 
      `data-toggle` = "dropdown",
      `data-value` = selected
    ))
  )
  dropTag <- tags$div(
    class = "dropdown-menu",
    class = if (isTRUE(dropPreScrollable)) "pre-scrollable",
    style = "padding: 5px;",
    style = if (!is.null(dropMaxHeight))
      paste0("max-height: ", validateCssUnit(dropMaxHeight), ";"),
    style = if (!is.null(dropWidth))
      paste0("width: ", validateCssUnit(dropWidth), ";"),
    lapply(
      X = seq_along(choicesNames),
      FUN = function(i) {
        do.call(
          what = tags$button,
          args = c(choicesNames[[i]], list(
            class = "btn drop-input-button",
            class = btnClass,
            style = "text-decoration: none !important;",
            `data-value` = choicesValues[i]
          ))
        )
      }
    )
  )
  tagList(
    singleton(
      tags$head(
        tags$script(src = "esquisse/drop/dropInput-bindings.js")
      )
    ),
    tags$div(
      id = inputId, class = "drop-input",
      class = ifelse(dropUp, "dropup", "dropdown"),
      btn, dropTag
    )
  )
}


#' Change the value of a drop input on the client
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param inputId The id of the input object.
#' @param selected The initially selected value.
#' @param disabled Choices (\code{choicesValues}) to disable.
#'
#' @export
#' 
#' @seealso \code{\link{dropInput}}
#'
#' @examples
#' if (interactive()) {
#'   
#'   library(shiny)
#'   library(esquisse)
#'   
#'   myChoices <- tagList(
#'     list(icon("home"), style = "width: 100px;"), 
#'     list(icon("flash"), style = "width: 100px;"),
#'     list(icon("cogs"), style = "width: 100px;"),
#'     list(icon("fire"), style = "width: 100px;"), 
#'     list(icon("users"), style = "width: 100px;"), 
#'     list(icon("info"), style = "width: 100px;")
#'   )
#'   
#'   
#'   ui <- fluidPage(
#'     tags$h2("Update Drop Input"),
#'     fluidRow(
#'       column(
#'         width = 6,
#'         dropInput(
#'           inputId = "mydrop",
#'           choicesNames = myChoices, 
#'           choicesValues = c("home", "flash", "cogs", "fire", "users", "info"),
#'           dropWidth = "220px"
#'         ),
#'         verbatimTextOutput(outputId = "res")
#'       ), 
#'       column(
#'         width = 6,
#'         actionButton("home", "Select home"),
#'         actionButton("flash", "Select flash"),
#'         actionButton("cogs", "Select cogs"),
#'         actionButton("fire", "Select fire"),
#'         actionButton("users", "Select users"),
#'         actionButton("info", "Select info"),
#'         checkboxGroupInput(
#'           inputId = "disabled",
#'           label = "Choices to disable",
#'           choices = c("home", "flash", "cogs", "fire", "users", "info")
#'         ),
#'         actionButton("disable", "Disable")
#'       )
#'     )
#'   )
#'   
#'   server <- function(input, output, session) {
#'     
#'     output$res <- renderPrint({
#'       input$mydrop
#'     })
#'     
#'     observeEvent(input$home, {
#'       updateDropInput(session, "mydrop", "home")
#'     })
#'     observeEvent(input$flash, {
#'       updateDropInput(session, "mydrop", "flash")
#'     })
#'     observeEvent(input$cogs, {
#'       updateDropInput(session, "mydrop", "cogs")
#'     })
#'     observeEvent(input$fire, {
#'       updateDropInput(session, "mydrop", "fire")
#'     })
#'     observeEvent(input$users, {
#'       updateDropInput(session, "mydrop", "users")
#'     })
#'     observeEvent(input$info, {
#'       updateDropInput(session, "mydrop", "info")
#'     })
#'     
#'     observeEvent(input$disable, {
#'       if (!is.null(input$disabled)) {
#'         updateDropInput(session, "mydrop", disabled = input$disabled)
#'       } else {
#'         updateDropInput(session, "mydrop", disabled = character(0))
#'       }
#'     })
#'   }
#'   
#'   shinyApp(ui, server)
#'   
#' }
updateDropInput <- function(session, inputId, selected = NULL, disabled = NULL) {
  if (!is.null(disabled) && length(disabled) == 1)
    disabled <- list(disabled)
  message <- dropNulls(list(
    selected = selected,
    disabled = disabled
  ))
  session$sendInputMessage(inputId, message)
}

