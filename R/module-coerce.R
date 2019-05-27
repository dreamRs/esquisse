
#' Coerce data.frame's columns module
#'
#' @param id Module id. See \code{\link[shiny]{callModule}}.
#' 
#' @name module-coerce
#'
#' @return a \code{reactiveValues} with two slots: \code{data} original \code{data.frame}
#'  with modified columns, and \code{names} column's names with call to coerce method.
#' @export
#' 
#' @importFrom htmltools tags
#' @importFrom shiny NS fluidRow column selectizeInput uiOutput actionButton icon
#' @importFrom shinyWidgets pickerInput
#'
#' @examples
#' 
#' if (interactive()) {
#'   library(esquisse)
#'   library(shiny)
#'   
#'   foo <- data.frame(
#'     num_as_char = as.character(1:10),
#'     char = sample(letters[1:3], 10, TRUE),
#'     fact = factor(sample(LETTERS[1:3], 10, TRUE)),
#'     date_as_char =  as.character(
#'       Sys.Date() + sample(seq(-10, 10), 10, TRUE)
#'     ),
#'     date_as_num = as.numeric(
#'       Sys.Date() + sample(seq(-10, 10), 10, TRUE)
#'     ),
#'     datetime = Sys.time() + sample(seq(-10, 10) * 1e4, 10, TRUE), 
#'     stringsAsFactors = FALSE
#'   )
#'   
#'   ui <- fluidPage(
#'     tags$h2("Coerce module"),
#'     fluidRow(
#'       column(
#'         width = 4,
#'         coerceUI(id = "example")
#'       ),
#'       column(
#'         width = 8,
#'         verbatimTextOutput(outputId = "print_result"),
#'         verbatimTextOutput(outputId = "print_names")
#'       )
#'     )
#'   )
#'   
#'   server <- function(input, output, session) {
#'     
#'     result <- callModule(module = coerceServer, id = "example", data = reactive({foo}))
#'     
#'     output$print_result <- renderPrint({
#'       str(result$data)
#'     })
#'     output$print_names <- renderPrint({
#'       result$names
#'     })
#'   }
#'   
#'   shinyApp(ui, server)
#' }
#' 
coerceUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    tags$style(
      ".col-coerce {padding-right: 5px; padding-left: 5px;}"
    ),
    useShinyUtils(),
    column(
      width = 5, class = "col-coerce",
      pickerInput(
        inputId = ns("var"),
        label = "Choose a variable to coerce:",
        choices = NULL,# names(data),
        multiple = FALSE,
        width = "100%"
      )
    ),
    column(
      width = 4, class = "col-coerce",
      selectizeInput(
        inputId = ns("coerce_to"),
        label = uiOutput(outputId = ns("coerce_to_label"), inline = FALSE, style = "min-height: 15px;"),
        choices = c("character", "factor", "numeric", "Date", "POSIXct"),
        multiple = FALSE,
        width = "100%"
      ),
      tags$div(
        id = ns("placeholder-date")
      )
    ),
    column(
      width = 3, class = "col-coerce",
      tags$div(
        style = "height: 25px;",
        tags$a(
          id = ns("help-coerce-vars"), style = "float: right;",
          style = "color: steelblue;", icon("info-circle", class = "fa-lg"),
          `data-toggle` = "popover", `data-trigger` = "hover", `data-animation` = "false",
          `data-container` = "body", tabindex = "0", role = "button",
          `data-content` = "Select a variable to change its class (for example to convert numbers into characters)",
          tags$script(sprintf("$('#%s').popover();", ns("help-coerce-vars")))
        )
      ),
      actionButton(
        inputId = ns("valid_coerce"),
        label = "Coerce",
        icon = icon("play"),
        width = "100%", 
        class = "btn-primary",
        disabled = "disabled"
      )
    )
  )
}


#' @param input,output,session standards \code{shiny} server arguments.²
#' @param data A \code{data.frame} or a \code{reactive}
#'  function returning a \code{data.frame} or a 
#'  \code{reactivevalues} with a slot containing a \code{data.frame} 
#'  (use \code{reactiveValuesSlot} to identify that slot)
#' @param reactiveValuesSlot If \code{data} is a \code{reactivevalues}, 
#'  specify the name of the slot containing data.
#'
#' @export
#' 
#' @rdname module-coerce
#' 
#' @importFrom htmltools tags
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom shiny reactiveValues renderUI observe removeUI insertUI
#'  textInput observeEvent showNotification updateActionButton icon
#'  is.reactivevalues is.reactive observe req
coerceServer <- function(input, output, session, data, reactiveValuesSlot = "data") {
  
  ns <- session$ns
  jns <- function(id) paste0("#", ns(id))
  
  return_data <- reactiveValues(data = NULL, names = NULL)
  
  observe({
    if (is.reactive(data)) {
      toggleInput(inputId = ns("valid_coerce"), enable = TRUE)
      toggleInput(inputId = ns("var"), enable = TRUE)
    } else if (is.reactivevalues(data) && !is.null(data[[reactiveValuesSlot]])) {
      toggleInput(inputId = ns("valid_coerce"), enable = TRUE)
      toggleInput(inputId = ns("var"), enable = TRUE)
    } else {
      toggleInput(inputId = ns("valid_coerce"), enable = FALSE)
      toggleInput(inputId = ns("var"), enable = FALSE)
    }
  })
  
  observe({
    req(data)
    if (is.reactive(data)) {
      data <- data()
    } else if (is.reactivevalues(data)) {
      req(data[[reactiveValuesSlot]])
      # data$timestamp
      data <- data[[reactiveValuesSlot]]
    }
    
    updatePickerInput(
      session = session,
      inputId = "var",
      choices = names(data),
      choicesOpt = list(
        subtext = unlist(lapply(
          X = data, FUN = function(x) class(x)[1]
        ), use.names = FALSE)
      )
    )
    return_data$data <- data
    return_data$names <- names(data)
  })
  
  output$coerce_to_label <- renderUI({
    req(return_data$data); req(input$var)
    if (input$var %in% names(return_data$data)) {
      var <- return_data$data[[input$var]]
      tags$span(
        "From", tags$code(class(var)[1]), "to:"
      )
    }
  })
  
  observe({
    req(return_data$data); req(input$var)
    if (input$var %in% names(return_data$data)) {
      data <- return_data$data
      removeUI(selector = jns("options-date"))
      classvar <- class(data[[input$var]])[1]
      if (input$coerce_to == "Date" & classvar %in% c("character", "factor")) {
        insertUI(
          selector = jns("placeholder-date"),
          ui = tags$div(
            id = ns("options-date"),
            textInput(
              inputId = ns("date_format"),
              label = "Specify format:",
              value = "%Y-%m-%d"
            )
          )
        )
      } else if (input$coerce_to == "Date" & classvar %in% c("numeric", "integer")) {
        insertUI(
          selector = jns("placeholder-date"),
          ui = tags$div(
            id = ns("options-date"),
            textInput(
              inputId = ns("date_origin"),
              label = "Specify origin:",
              value = "1970-01-01"
            )
          )
        )
      } else if (input$coerce_to == "POSIXct" & classvar %in% c("character", "factor")) {
        insertUI(
          selector = jns("placeholder-date"),
          ui = tags$div(
            id = ns("options-date"),
            textInput(
              inputId = ns("posixct_format"),
              label = "Specify format:",
              value = "%Y-%m-%d %H:%M:%S"
            )
          )
        )
      } else if (input$coerce_to == "POSIXct" & classvar %in% c("numeric", "integer")) {
        insertUI(
          selector = jns("placeholder-date"),
          ui = tags$div(
            id = ns("options-date"),
            textInput(
              inputId = ns("posixct_origin"),
              label = "Specify origin:",
              value = "1970-01-01 00:00:00"
            )
          )
        )
      }
    }
  })
  
  observeEvent(input$valid_coerce, {
    var <- return_data$data[[input$var]]
    classvar <- class(var)[1]
    args <- list(x = var)
    argsup <- ""
    if (input$coerce_to %in% "Date") {
      if (classvar %in% c("numeric", "integer")) {
        args$origin <- input$date_origin
        argsup <- sprintf(", origin = \"%s\"", input$date_origin)
      } else {
        args$format <- input$date_format
        argsup <- sprintf(", format = \"%s\"", input$date_format)
      }
    } else if (input$coerce_to %in% "POSIXct") {
      if (classvar %in% c("numeric", "integer")) {
        args$origin <- input$posixct_origin
        argsup <- sprintf(", origin = \"%s\"", input$posixct_origin)
      } else {
        args$format <- input$posixct_format
        argsup <- sprintf(", format = \"%s\"", input$posixct_format)
      }
    }
    var <- withCallingHandlers(
      expr = tryCatch(
        expr = {
          do.call(what = paste0("as.", input$coerce_to), args = args)
        },
        error = function(e) {
          shiny::showNotification(ui = conditionMessage(e), type = "error", session = session)
        }
      ), 
      warning = function(w) {
        shiny::showNotification(ui = conditionMessage(w), type = "warning", session = session)
      }
    )
    return_data$data[[input$var]] <- var
    return_data$names <- replace(
      x = return_data$names, 
      list = which(return_data$names == input$var),
      values = sprintf("as.%s(%s%s)", input$coerce_to, input$var, argsup)
    )
    updateActionButton(
      session = session, 
      inputId = "valid_coerce",
      label = "Coerced !",
      icon = icon("check")
    )
    session$sendCustomMessage(
      type = "toggleClass",
      message = list(id = ns("valid_coerce"), class = "success")
    )
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$var, input$coerce_to), {
    updateActionButton(
      session = session, 
      inputId = "valid_coerce",
      label = "Coerce",
      icon = icon("play")
    )
    session$sendCustomMessage(
      type = "toggleClass",
      message = list(id = ns("valid_coerce"), class = "primary")
    )
  }, ignoreInit = TRUE)
  
  return(return_data)
}





