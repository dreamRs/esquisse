
#' Modules for creating filters from a data.frame
#'
#' @param id Module's id
#'
#' @return a \code{\link[shiny]{reactiveValues}} containing the data filtered under 
#' slot \code{data}, the R code to reproduce the filtering under slot \code{code} and a logical
#' vector for indexing data under slot \code{index}.
#' @export
#' @importFrom htmltools tags
#' @importFrom shiny NS
#' 
#' @name filterData-module
#'
#' @examples
#' 
#' \dontrun{
#' 
#' if (interactive()) {
#' library(shiny)
#' library(shinyWidgets)
#' library(esquisse)
#' 
#' ui <- fluidPage(
#'   
#'   tags$h1("Module Filter Data"),
#'   
#'   fluidRow(
#'     column(
#'       width = 4,
#'       radioButtons(
#'         inputId = "dataset", label = "Data:",
#'         choices = c("iris", "mtcars", "Titanic")
#'       ),
#'       filterDataUI("ex")
#'     ),
#'     column(
#'       width = 8,
#'       progressBar(
#'         id = "pbar", value = 100, 
#'         total = 100, display_pct = TRUE
#'       ),
#'       DT::dataTableOutput(outputId = "tab"),
#'       verbatimTextOutput(outputId = "code")
#'     )
#'   )
#'   
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   data <- reactive({
#'     if (input$dataset == "iris") {
#'       return(iris)
#'     } else if (input$dataset == "mtcars") {
#'       return(mtcars)
#'     } else {
#'       return(as.data.frame(Titanic))
#'     }
#'   })
#'   
#'   res <- callModule(module = filterDataServer, 
#'                     id = "ex", data = data)
#'   
#'   observeEvent(res$data, {
#'     updateProgressBar(
#'       session = session, id = "pbar", 
#'       value = nrow(res$data), total = nrow(data())
#'     )
#'   })
#'   
#'   output$tab <- DT::renderDataTable(res$data)
#'   
#'   output$code <- renderPrint(res$code)
#'   
#' }
#' 
#' shinyApp(ui, server)
#' }
#' 
#' }
#' 
filterDataUI <- function(id) {
  ns <- NS(id)
  tags$div(id = ns("placeholder-filters"))
}



#' @param input standard \code{shiny} input.
#' @param output standard \code{shiny} output.
#' @param session standard \code{shiny} session.
#' @param data a \code{data.frame} or a \code{\link[shiny]{reactive}} function returning a \code{data.frame}.
#' @param vars variables for which to create filters, by default all variables in \code{data}.
#' @param width the width of the input, e.g. \code{400px}, or \code{100\%}.
#'
#' @export
#' 
#' @rdname filterData-module
#'
#' @importFrom shiny reactiveValues reactive is.reactive observeEvent removeUI insertUI reactiveValuesToList
filterDataServer <- function(input, output, session, data, vars = NULL, width = "100%") {
  
  ns <- session$ns
  jns <- function(id) paste0("#", ns(id))
  key <- reactiveValues(x = NULL, index = TRUE)
  
  return_data <- reactiveValues(data = NULL)
  
  data_filter <- reactive({
    if (is.reactive(data)) {
      dat_ <- as.data.frame(data())
    } else {
      dat_ <- as.data.frame(data)
    }
    if (is.null(vars)) {
      vars <- names(dat_)
    }
    dat_ <- dat_[, vars, drop = FALSE]
    key$x <- paste(sample(letters, 10, TRUE), collapse = "")
    return_data$data <- dat_
    return_data$code <- ""
    return_data$index <- rep_len(TRUE, nrow(dat_))
    return(dat_)
  })
  
  observeEvent(data_filter(), {
    data <- data_filter()
    tagFilt <- lapply(
      X = names(data), FUN = create_input_filter, 
      data = data, ns = ns, key = key$x,
      width = width
    )
    removeUI(selector = jns("filters-mod"))
    insertUI(
      selector = jns("placeholder-filters"),
      ui = tags$div(
        id = ns("filters-mod"), tagFilt
      )
    )
  })
  
  
  observeEvent(reactiveValuesToList(input), {
    params <- reactiveValuesToList(input)
    params <- params[grep(x = names(params), pattern = key$x)]
    names(params) <- gsub(pattern = paste0(key$x, "_"), replacement = "", x = names(params))
    data <- data_filter()
    res_f <- lapply(
      X = names(params),
      FUN = generate_filters,
      data = data, params = params
    )
    res_len <- unlist(lapply(res_f, length))
    if (sum(res_len) > 0) {
      ind <- Reduce(`&`, lapply(res_f, `[[`, "ind"))
      code <- Reduce(`%+%`, lapply(res_f, `[[`, "code"))
      return_data$index <- ind
      return_data$code <- code
      return_data$data <- data[ind, ]
    }
  }, ignoreInit = TRUE)
  
  
  return(return_data)
}





#' @importFrom shiny sliderInput selectizeInput
create_input_filter <- function(data, var, ns, key = "filter", width = "100%") {
  x <- data[[var]]
  if (inherits(x = x, what = c("numeric", "integer"))) {
    x <- x[!is.na(x)]
    rangx <- range(x)
    if (diff(rangx) < 4) {
      step <- pretty(rangx, n = 100)
      step <- step[2] - step[1]
      step <- as.numeric(prettyNum(step))
    } else {
      step <- 1
    }
    sliderInput(
      inputId = ns(paste(key, var, sep = "_")), label = var, 
      min = min(x), max = max(x), width = width,
      value = rangx, step = step
    )
  } else if (inherits(x = x, what = c("Date", "POSIXct"))) {
    x <- x[!is.na(x)]
    rangx <- range(x)
    sliderInput(
      inputId = ns(paste(key, var, sep = "_")), label = var, 
      min = min(x), max = max(x), width = width,
      value = rangx
    )
  } else {
    x <- unique(x[!is.na(x)])
    selectizeInput(
      inputId = ns(paste(key, var, sep = "_")), label = var,
      choices = x, selected = x, 
      multiple = TRUE, width = width,
      options = list(plugins = list("remove_button"))
    )
  }
}


# # @importFrom shiny updateSelectizeInput
# update_selectize <- function(session, data, var, key = "filter") {
#   x <- data[[var]]
#   if (!inherits(x = x, what = c("numeric", "integer"))) {
#     x <- unique(x[!is.na(x)])
#     updateSelectizeInput(
#       session = session, inputId = paste(key, var, sep = "_"),
#       choices = x, selected = x
#     )
#   }
# }



num_equal <- function(x, y, tol = sqrt(.Machine$double.eps)) {
  abs(x - y) < tol
}

`%+%` <- function(e1, e2) {
  if (e1 != "" & e2 != "") {
    paste(e1, e2, sep = " & ")
  } else if (e1 != "" & e2 == "") {
    e1
  } else if (e1 == "" & e2 != "") {
    e2
  } else {
    ""
  }
}




generate_filters <- function(x, params, data) {
  if (x %in% names(data)) {
    values <- params[[x]]
    dat <- data[[x]]
    if (inherits(x = dat, what = c("numeric", "integer"))) {
      if (!num_equal(min(dat), values[1])) {
        code1 <- sprintf("%s >= %s", x, values[1])
      } else {
        code1 <- ""
      }
      if (!num_equal(max(dat), values[2])) {
        code2 <- sprintf("%s <= %s", x, values[2])
      } else {
        code2 <- ""
      }
      list(
        code = code1 %+% code2,
        ind = dat >= values[1] & dat <= values[2]
      )
    } else if (inherits(x = dat, what = "Date")) {
      if (min(dat) != values[1]) {
        code1 <- sprintf("%s >= as.Date('%s')", x, values[1])
      } else {
        code1 <- ""
      }
      if (max(dat) != values[2]) {
        code2 <- sprintf("%s <= as.Date('%s')", x, values[2])
      } else {
        code2 <- ""
      }
      list(
        code = code1 %+% code2,
        ind = dat >= values[1] & dat <= values[2]
      )
    } else if (inherits(x = dat, what = "POSIXct")) {
      if (min(dat) != values[1]) {
        code1 <- sprintf("%s >= as.POSIXct('%s')", x, values[1])
      } else {
        code1 <- ""
      }
      if (max(dat) != values[2]) {
        code2 <- sprintf("%s <= as.POSIXct('%s')", x, values[2])
      } else {
        code2 <- ""
      }
      list(
        code = code1 %+% code2,
        ind = dat >= values[1] & dat <= values[2]
      )
    } else {
      if (all(unique(dat) %in% values)) {
        code <- ""
      } else {
        code <- sprintf("%s %%in%% c(%s)", x, paste(sprintf("'%s'", values), collapse = ", "))
      }
      list(
        code = code,
        ind = dat %in% values
      )
    }
  }
}


