
#' Module to import a file
#'
#' @param id Module id. See \code{\link[shiny]{callModule}}.
#' @param dismissOnValidate Dismiss modal when user validate data, \code{TRUE} by default.
#' @param selectVars Display module to select variables, \code{TRUE} by default.
#' @param coerceVars Display module to coerce variables between different class, \code{TRUE} by default.
#'
#' @noRd
#' 
#' @name data-import-file
#' 
#' @importFrom htmltools tagList tags HTML
#' @importFrom shiny NS fileInput actionButton icon
#'
dataImportFileUI <- function(id, dismissOnValidate = TRUE, selectVars = TRUE, coerceVars = TRUE) {
  ns <- NS(id)
  
  tagList(
    useShinyUtils(),
    tags$script(
      sprintf("Shiny.onInputChange('%s', %f);", ns("resetImportFile"), as.numeric(Sys.time()))
    ),
    tags$h2("Import a dataset"),
    fileInput(
      inputId = ns("file"), 
      label = "Choose a file:", 
      accept = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat", ".sav"),
      width = "100%"
    ),
    tags$div(
      id = ns("placeholder-result-import"),
      tags$div(
        id = ns("result-import"), class = "alert alert-info",
        tags$b("No file"), "Import .rds, .txt, .csv, .xls, .xlsx, .sas7bdat, .sav, ..."
      )
    ),
    
    tags$div(
      style = if (!isTRUE(selectVars)) "display: none;",
      tags$br(),
      selectVarsUI(id = ns("selected"))
    ),
    tags$div(
      style = if (!isTRUE(coerceVars)) "display: none;",
      style = "margin: 10px;",
      tags$br(),
      tags$br(),
      coerceUI(id = ns("coerce"))
    ),
    
    tags$br(), tags$br(), 
    actionButton(
      inputId = ns("validate"),
      label = "Validate imported data",
      icon = icon("arrow-circle-right"),
      width = "100%", disabled = "disabled",
      class = "btn-primary", 
      `data-dismiss` = if (isTRUE(dismissOnValidate)) "modal" else NULL
    )
  )
}



#' @param input,output,session standards \code{shiny} server arguments.
#' @param data Default \code{data.frame} to use.
#' @param name Default name to use.
#' 
#' @noRd
#' 
#' @rdname data-import-file
#'
# @importFrom rio import
#' @importFrom shiny reactiveValues observeEvent removeUI insertUI callModule
dataImportFileServer <- function(input, output, session, data = NULL, name = NULL) {
  
  if (!requireNamespace(package = "rio", quietly = TRUE))
    message("Package 'rio' is required to run this function")
  
  ns <- session$ns
  jns <- function(x) paste0("#", ns(x))
  
  imported_data <- reactiveValues(data = NULL, name = NULL)
  tmp_name <- reactiveValues(name = NULL)
  select_data <- reactiveValues(data = NULL, name = NULL, timestamp = Sys.time())
  coerce_data <- reactiveValues(data = NULL, name = NULL, timestamp = Sys.time())
  
  observeEvent(input$resetImportFile, {
    imported_data$data <- NULL
    imported_data$name <- NULL
  })
  
  observeEvent(input$file, {
    imported <- try(rio::import(file = input$file$datapath), silent = TRUE)
    if ("try-error" %in% class(imported) || NROW(imported) < 1) {
      toggleInput(inputId = ns("validate"), enable = FALSE)
      removeUI(selector = jns("result-import"))
      insertUI(
        selector = jns("placeholder-result-import"),
        ui = tags$div(
          id = ns("result-import"), class = "alert alert-danger",
          tags$b("Failure"), "the file could not be read"
        )
      )
      select_data$data <- NULL
      coerce_data$data <- NULL
      select_data$timestamp <- Sys.time()
    } else {
      toggleInput(inputId = ns("validate"), enable = TRUE)
      removeUI(selector = jns("result-import"))
      insertUI(
        selector = jns("placeholder-result-import"),
        ui = tags$div(
          id = ns("result-import"), class = "alert alert-success",
          tags$b("Success"), 
          sprintf("%s obs. of %s variables imported", 
                  nrow(imported), ncol(imported))
        )
      )
      tmp_name$name <- sprintf("rio::import('%s')", input$file$name)
      select_data$data <- imported
      coerce_data$data <- imported
      select_data$timestamp <- Sys.time()
    }
  }, ignoreInit = TRUE)
  
  
  sv <- callModule(module = selectVarsServer, id = "selected", data = select_data)
  
  observeEvent(sv$selected_vars, {
    if (length(sv$selected_vars) > 0) {
      toggleInput(inputId = ns("validate"), enable = TRUE)
      coerce_data$data <- select_data$data[, sv$selected_vars, drop = FALSE]
    } else {
      toggleInput(inputId = ns("validate"), enable = FALSE)
    }
  }, ignoreNULL = FALSE)
  
  
  coerced_data <- callModule(module = coerceServer, id = "coerce", data = coerce_data)
  
  observeEvent(input$validate, {
    if (!is.null(coerced_data$data)) {
      dat <- coerced_data$data
    } else {
      dat <- select_data$data
    }
    imported_data$name <- tmp_name$name
    imported_data$data <- dat
  })
  
  return(imported_data)
}


