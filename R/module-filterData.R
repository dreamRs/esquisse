
#' Modules for creating filters from a data.frame
#'
#' @param id Module's id
#'
#' @return a \code{\link[shiny]{reactiveValues}} containing the data filtered under the slot \code{data}.
#' @export
#' 
#' @name filterData-module
#'
#' @examples
#' 
#' \dontrun{
#' 
#' if (interactive()) {
#'   library(shiny)
#'   
#'   ui <- fluidPage(
#'     
#'     tags$h1("Module Filter Data"),
#'     
#'     fluidRow(
#'       column(
#'         width = 4,
#'         radioButtons(
#'           inputId = "dataset", label = "Data:",
#'           choices = c("iris", "mtcars", "Titanic")
#'         ),
#'         filterDataUI("ex")
#'       ),
#'       column(
#'         width = 8,
#'         DT::dataTableOutput(outputId = "tab")
#'       )
#'     )
#'     
#'   )
#'   
#'   server <- function(input, output, session) {
#'     
#'     data <- reactive({
#'       if (input$dataset == "iris") {
#'         return(iris)
#'       } else if (input$dataset == "mtcars") {
#'         return(mtcars)
#'       } else {
#'         return(as.data.frame(Titanic))
#'       }
#'     })
#'     
#'     res <- callModule(module = filterDataServer, 
#'                       id = "ex", data = data)
#'     
#'     output$tab <- DT::renderDataTable(res$data)
#'     
#'   }
#'   
#'   shinyApp(ui, server)
#' }
#' 
#' }
#' 
#' @importFrom shiny NS tags
filterDataUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(id = ns("placeholder-filters"))
}



#' @param input standard \code{shiny} input.
#' @param output standard \code{shiny} output.
#' @param session standard \code{shiny} session.
#' @param data a \code{data.frame} or a \code{\link[shiny]{reactive}} function returning a \code{data.frame}.
#' @param vars variables for which to create filters, by default all variables in \code{data}.
#'
#' @export
#' 
#' @rdname filterData-module
#'
#' @importFrom shiny reactiveValues reactive is.reactive observeEvent removeUI insertUI tags reactiveValuesToList
filterDataServer <- function(input, output, session, data, vars = NULL) {
  
  ns <- session$ns
  jns <- function(id) paste0("#", ns(id))
  key <- reactiveValues(x = NULL)
  
  return_data <- shiny::reactiveValues(data = NULL)
  
  data_filter <- shiny::reactive({
    if (shiny::is.reactive(data)) {
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
    return(dat_)
  })
  
  shiny::observeEvent(data_filter(), {
    data <- data_filter()
    tagFilt <- lapply(
      X = names(data), FUN = create_input_filter, 
      data = data, ns = ns, key = key$x
    )
    shiny::removeUI(selector = jns("filters-mod"))
    shiny::insertUI(
      selector = jns("placeholder-filters"),
      ui = shiny::tags$div(
        id = ns("filters-mod"), tagFilt
      )
    )
  })
  
  
  shiny::observeEvent(shiny::reactiveValuesToList(input), {
    params <- shiny::reactiveValuesToList(input)
    params <- params[grep(x = names(params), pattern = key$x)]
    names(params) <- gsub(pattern = paste0(key$x, "_"), replacement = "", x = names(params))
    data <- data_filter()
    res_f <- lapply(
      X = names(params),
      FUN = function(x) {
        if (x %in% names(data)) {
          values <- params[[x]]
          dat <- data[[x]]
          if (inherits(x = dat, what = c("numeric", "integer"))) {
            dat >= values[1] & dat <= values[2]
          } else {
            dat %in% values
          }
        }
      }
    )
    ind <- Reduce(`&`, res_f)
    return_data$data <- data[ind, ]
  }, ignoreInit = TRUE)
  
  
  return(return_data)
}





#' @importFrom shiny sliderInput selectizeInput
create_input_filter <- function(data, var, ns, key = "filter") {
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
    shiny::sliderInput(
      inputId = ns(paste(key, var, sep = "_")), label = var, 
      min = min(x), max = max(x), width = "100%",
      value = rangx, step = step
    )
  } else {
    x <- unique(x[!is.na(x)])
    shiny::selectizeInput(
      inputId = ns(paste(key, var, sep = "_")), label = var,
      choices = x, selected = x, multiple = TRUE, width = "100%",
      options = list(plugins = list("remove_button"))
    )
  }
}

