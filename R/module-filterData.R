
#' Modules for creating filters from a data.frame
#'
#' @param id Module's id
#'
#' @return a \code{\link[shiny]{reactiveValues}} containing the data filtered under 
#' slot \code{data}, the R code to reproduce the filtering under slot \code{code} and a logical
#' vector for indexing data under slot \code{index}.
#' @export
#' @importFrom htmltools tags tagList 
#' @importFrom shiny NS singleton
#' 
#' @name filterData-module
#' 
#' @note Column's names can be modified to be valid R names.
#'  discrete columns with more than 50 unique values will be discarded. 
#'
#' @examples
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
filterDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    singleton(
      tags$style(".selectize-big .selectize-input {height: 72px; overflow-y: scroll;}")
    ),
    tags$div(id = ns("placeholder-filters"))
  )
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
    old_names <- names(dat_)
    names(dat_) <- clean_string(old_names)
    if (is.null(vars)) {
      vars <- names(dat_)
    }
    dat_ <- dat_[, vars, drop = FALSE]
    key$x <- paste(sample(letters, 10, TRUE), collapse = "")
    return_data$data <- dat_
    return_data$code <- ""
    return_data$index <- rep_len(TRUE, nrow(dat_))
    return_data$old_names <- old_names
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
    params_na <- params[grep(x = names(params), pattern = "na_remove")]
    names(params_na) <- gsub(pattern = paste0(key$x, "_"), replacement = "", x = names(params_na))
    names(params_na) <- gsub(pattern = "_na_remove", replacement = "", x = names(params_na))
    params <- params[-grep(x = names(params), pattern = "na_remove")]
    names(params) <- gsub(pattern = paste0(key$x, "_"), replacement = "", x = names(params))
    data <- data_filter()
    res_f <- lapply(
      X = names(params),
      FUN = generate_filters,
      data = data, params = params,
      params_na = params_na
    )
    res_len <- unlist(lapply(res_f, length))
    if (sum(res_len) > 0) {
      ind <- Reduce(`&`, lapply(res_f, `[[`, "ind"))
      code <- Reduce(`%+&%`, lapply(res_f, `[[`, "code"))
      return_data$index <- ind
      return_data$code <- code
      return_data$data <- data[ind, ]
    }
  }, ignoreInit = TRUE)
  
  
  return(return_data)
}



#' @importFrom htmltools tagList HTML
#' @importFrom shinyWidgets sliderTextInput prettySwitch prettyToggle pickerInput
#' @importFrom shiny sliderInput selectizeInput splitLayout
create_input_filter <- function(data, var, ns, key = "filter", width = "100%") {
  x <- data[[var]]
  if (inherits(x = x, what = c("numeric", "integer"))) {
    x <- x[!is.na(x)]
    if (length(x) == 0)
      return(NULL)
    if (num_equal(max(x), min(x))) {
      return(NULL)
    } else {
      if (length(unique(x)) <= 30) {
        values <- sort(unique(x))
      } else {
        values <- seq(from = min(x), to = max(x), length.out = 30)
      }
      values <- range_val(values)
      tagList(
        tags$span(
          tags$label(var), HTML("&nbsp;&nbsp;"), naInput(key, clean_string(var), ns) 
        ),
        sliderTextInput(
          inputId = ns(paste(key, clean_string(var), sep = "_")), label = NULL,
          choices = values, selected = range(values), 
          force_edges = TRUE, grid = TRUE, width = width
        )
      )
    }
  } else if (inherits(x = x, what = c("Date", "POSIXct"))) {
    x <- x[!is.na(x)]
    if (length(x) == 0)
      return(NULL)
    rangx <- range(x)
    tagList(
      tags$span(
        tags$label(var), HTML("&nbsp;&nbsp;"), naInput(key, clean_string(var), ns) 
      ),
      sliderInput(
        inputId = ns(paste(key, clean_string(var), sep = "_")), 
        min = min(x), max = max(x), width = width,
        value = rangx, label = NULL
      )
    )
  } else if (inherits(x = x, what = c("character", "factor"))) {
    x <- x[!is.na(x) & trimws(x) != ""]
    if (length(x) == 0)
      return(NULL)
    if (length(unique(x)) >= length(x) * 0.9)
      return(NULL)
    if (length(unique(x)) == 1)
      return(NULL)
    if (length(unique(x)) >= 50)
      return(NULL)
    if (inherits(x, "factor")) {
      x <- levels(x)
    } else {
      x <- unique(x)
    }
    
    tags$div(
      class = if (length(x) > 15) "selectize-big",
      tags$span(
        tags$label(var), HTML("&nbsp;&nbsp;"), naInput(key, clean_string(var), ns) 
      ),
      selectizeInput(
        inputId = ns(paste(key, clean_string(var), sep = "_")),
        choices = x, selected = x, label = NULL,
        multiple = TRUE, width = width,
        options = list(plugins = list("remove_button"))
      )
    )
  } else {
    NULL
  }
}

naInput <- function(key, var, ns) {
  prettyToggle(
    inputId = ns(paste(key, var, "na_remove", sep = "_")), 
    value = TRUE,
    label_on = "NA", icon_on = icon("ok", lib = "glyphicon"),
    label_off = "NA", icon_off = icon("remove", lib = "glyphicon"),
    status_on = "success", status_off = "danger",
    inline = TRUE
  )
}


range_val <- function(x) {
  y <- round(x, 2)
  y[1] <- trunc(x[1]*1000)/1000
  y[length(y)] <- ceiling(x[length(x)]*1000)/1000
  return(y)
}

num_equal <- function(x, y, tol = sqrt(.Machine$double.eps)) {
  abs(x - y) < tol
}





#' @importFrom stats na.omit
generate_filters <- function(x, params, params_na, data) {
  if (x %in% names(data)) {
    values <- params[[x]]
    if (is.null(values)) {
      list(
        code = "",
        ind = rep_len(TRUE, length(dat))
      )
    }
    dat <- data[[x]]
    na <- params_na[[x]]
    if (is.null(na)) {
      na <- FALSE
    }
    if (inherits(x = dat, what = c("numeric", "integer"))) {
      if (!isTRUE(num_equal(min(dat, na.rm = TRUE), values[1], tol = 0.009))) {
        code1 <- sprintf("%s >= %s", x, values[1])
      } else {
        code1 <- ""
      }
      if (!isTRUE(num_equal(max(dat, na.rm = TRUE), values[2], tol = 0.009))) {
        code2 <- sprintf("%s <= %s", x, values[2])
      } else {
        code2 <- ""
      }
      if (na) {
        codena <- sprintf("| is.na(%s)", x)
        ind <- (dat >= values[1] & dat <= values[2]) | is.na(dat)
      } else {
        codena <- sprintf("& !is.na(%s)", x)
        ind <- dat >= values[1] & dat <= values[2] & !is.na(dat)
      }
      list(
        code = code1 %+&% code2 %+1% codena,
        ind = ind
      )
    } else if (inherits(x = dat, what = "Date")) {
      if (isTRUE(min(dat, na.rm = TRUE) != values[1])) {
        code1 <- sprintf("%s >= as.Date('%s')", x, values[1])
      } else {
        code1 <- ""
      }
      if (isTRUE(max(dat, na.rm = TRUE) != values[2])) {
        code2 <- sprintf("%s <= as.Date('%s')", x, values[2])
      } else {
        code2 <- ""
      }
      if (na) {
        codena <- sprintf("| is.na(%s)", x)
        ind <- (dat >= values[1] & dat <= values[2]) | is.na(dat)
      } else {
        codena <- sprintf("& !is.na(%s)", x)
        ind <- dat >= values[1] & dat <= values[2] & !is.na(dat)
      }
      list(
        code = code1 %+&% code2 %+1% codena,
        ind = ind
      )
    } else if (inherits(x = dat, what = "POSIXct")) {
      if (isTRUE(min(dat, na.rm = TRUE) != values[1])) {
        code1 <- sprintf("%s >= as.POSIXct('%s')", x, values[1])
      } else {
        code1 <- ""
      }
      if (isTRUE(max(dat, na.rm = TRUE) != values[2])) {
        code2 <- sprintf("%s <= as.POSIXct('%s')", x, values[2])
      } else {
        code2 <- ""
      }
      if (na) {
        codena <- sprintf("| is.na(%s)", x)
        ind <- (dat >= values[1] & dat <= values[2]) | is.na(dat)
      } else {
        codena <- sprintf("& !is.na(%s)", x)
        ind <- dat >= values[1] & dat <= values[2] & !is.na(dat)
      }
      list(
        code = code1 %+&% code2 %+1% codena,
        ind = ind
      )
    } else if (inherits(x = dat, what = c("character", "factor"))) {
      dat[trimws(dat) == ""] <- NA
      if (all(unique(na.omit(dat)) %in% values)) {
        code <- ""
        if (na) {
          codena <- ""
          ind <- rep_len(TRUE, length(dat))
        } else {
          codena <- sprintf("!is.na(%s)", x)
          ind <- !is.na(dat)
        }
      } else {
        code <- sprintf("%s %%in%% c(%s)", x, paste(sprintf("'%s'", values), collapse = ", "))
        if (na) {
          codena <- sprintf("| is.na(%s)", x)
          ind <- dat %in% values | is.na(dat)
        } else {
          codena <- ""
          ind <- dat %in% values
        }
      }
      list(
        code = code %+% codena,
        ind = ind
      )
    } else {
      list(
        code = "",
        ind = rep_len(TRUE, length(dat))
      )
    }
  }
}


