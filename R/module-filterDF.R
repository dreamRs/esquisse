
#' @title Shiny module to interactively filter a \code{data.frame}
#' 
#' @description Module generate inputs to filter \code{data.frame} according column's type.
#'  Code to reproduce the filter is returned as an expression with filtered data.
#'
#' @param id Module id. See \code{\link[shiny]{callModule}}.
#' @param show_nrow Show number of filtered rows and total.
#'
#' @return A \code{list} with 2 elements :
#'  \itemize{
#'   \item \strong{data_filtered} : \code{\link[shiny]{reactive}} function returning data filtered.
#'   \item \strong{code} : \code{\link[shiny]{reactiveValues}} with 2 slots :
#'    \code{expr} (raw expression to filter data) and \code{dplyr} (code with dplyr pipeline).
#'  }
#'  
#' @export
#' 
#' @name module-filterDF
#' 
#' @importFrom htmltools tagList singleton tags
#' @importFrom shiny NS uiOutput
#'
#' @examples
#' if (interactive()) {
#' 
#' library(shiny)
#' library(shinyWidgets)
#' library(ggplot2)
#' library(esquisse)
#' 
#' 
#' ui <- fluidPage(
#'   tags$h2("Filter data.frame"),
#'   
#'   radioButtons(
#'     inputId = "dataset", 
#'     label = "Data:",
#'     choices = c(
#'       "iris", "mtcars", "economics", 
#'       "midwest", "mpg", "msleep", "diamonds",
#'       "faithfuld", "txhousing"
#'     ),
#'     inline = TRUE
#'   ),
#'   
#'   fluidRow(
#'     column(
#'       width = 3,
#'       filterDF_UI("filtering")
#'     ),
#'     column(
#'       width = 9,
#'       progressBar(
#'         id = "pbar", value = 100, 
#'         total = 100, display_pct = TRUE
#'       ),
#'       DT::dataTableOutput(outputId = "table"),
#'       tags$p("Code dplyr:"),
#'       verbatimTextOutput(outputId = "code_dplyr"),
#'       tags$p("Expression:"),
#'       verbatimTextOutput(outputId = "code"),
#'       tags$p("Filtered data:"),
#'       verbatimTextOutput(outputId = "res_str")
#'     )
#'   )
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   data <- reactive({
#'     get(input$dataset)
#'   })
#'   
#'   res_filter <- callModule(
#'     module = filterDF, 
#'     id = "filtering", 
#'     data_table = data,
#'     data_name = reactive(input$dataset)
#'   )
#'   
#'   observeEvent(res_filter$data_filtered(), {
#'     updateProgressBar(
#'       session = session, id = "pbar", 
#'       value = nrow(res_filter$data_filtered()), total = nrow(data())
#'     )
#'   })
#'   
#'   output$table <- DT::renderDT({
#'     res_filter$data_filtered()
#'   }, options = list(pageLength = 5))
#'   
#'   
#'   output$code_dplyr <- renderPrint({
#'     res_filter$code$dplyr
#'   })
#'   output$code <- renderPrint({
#'     res_filter$code$expr
#'   })
#'   
#'   output$res_str <- renderPrint({
#'     str(res_filter$data_filtered())
#'   })
#'   
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }
filterDF_UI <- function(id, show_nrow = TRUE) {
  ns <- NS(id)
  tagList(
    singleton(
      tags$style(
        ".selectize-big .selectize-input {height: 72px; overflow-y: scroll;}"
      )
    ),
    if (isTRUE(show_nrow)) uiOutput(outputId = ns("nrow")),
    tags$div(id = ns("placeholder-filters"))
  )
}

#' @param input,output,session standards \code{shiny} server arguments.
#' @param data_table \code{\link[shiny]{reactive}} function returning a
#'  \code{data.frame} to filter.
#' @param data_vars \code{\link[shiny]{reactive}} function returning a
#'  \code{character} vector of variable to use for filters.
#' @param data_name \code{\link[shiny]{reactive}} function returning a
#'  \code{character} string representing \code{data_table} name.
#' 
#' 
#' @rdname module-filterDF
#' @export
#'
#' @importFrom rlang eval_tidy
#' @importFrom shiny observeEvent reactiveValues removeUI
#'  insertUI reactive req isolate reactive renderUI tags
filterDF <- function(input, output, session, 
                     data_table = reactive(), 
                     data_vars = shiny::reactive(NULL),
                     data_name = reactive("data")) {
  
  ns <- session$ns
  jns <- function(x) paste0("#", ns(x))
  
  output$nrow <- renderUI({
    tags$p("Number of rows: ", tags$b(nrow(data_filtered()) , "/", nrow(data_table())))
  })
  
  rv_filters <- reactiveValues(mapping = NULL, mapping_na = NULL)
  rv_code <- reactiveValues(expr = NULL, dplyr = NULL)
  
  observeEvent(data_table(), {
    data <- data_table()
    vars <- data_vars()
    # req(nrow(data) > 0)
    removeUI(selector = jns("filters_inputs"), immediate = TRUE)
    filters <- create_filters(data = data, vars = vars)
    insertUI(
      selector = jns("placeholder-filters"), 
      ui = tags$div(
        id = ns("filters_inputs"),
        filters$ui
      ), 
      immediate = TRUE
    )
    rv_filters$mapping <- filters$filters_id
    rv_filters$mapping_na <- filters$filters_na_id
  })
  
  data_filtered <- reactive({
    data <- data_table()
    req(all(names(rv_filters$mapping) %in% names(data)))
    filter_inputs <- lapply(
      X = rv_filters$mapping, 
      FUN = function(x) {
        req(input[[x]])
        input[[x]]
      }
    )
    filter_nas <- lapply(
      X = rv_filters$mapping_na,
      FUN = function(x) {
        input[[x]]
      }
    )
    filters <- make_expr_filter(
      filters = filter_inputs, 
      filters_na = filter_nas,
      data = data,
      data_name = isolate(data_name())
    )
    rv_code$expr <- filters$expr
    rv_code$dplyr <- filters$expr_dplyr
    if (length(rv_code$expr) > 0) {
      result <- eval_tidy(expr = rv_code$expr, data = data)
      data[result, ]
    } else {
      data
    }
  })
  
  list(
    data_filtered = data_filtered,
    code = rv_code
  )
}



# Utils -------------------------------------------------------------------




#' @importFrom htmltools HTML tagList tags
#' @importFrom shiny selectizeInput sliderInput
#' @importFrom stats setNames
create_filters <- function(data, vars = NULL, width = "100%", session = getDefaultReactiveDomain()) {
  ns <- session$ns
  data <- drop_id(data)
  data <- drop_na(data)
  data <- dropListColumns(data)
  if (is.null(vars)) 
    vars <- names(data)
  filters_id <- paste0("filter_", sample.int(1e9, length(vars)))
  filters_id <- setNames(as.list(filters_id), vars)
  filters_na_id <- setNames(as.list(paste0("na_", filters_id)), vars)
  ui <- lapply(
    X = vars,
    FUN = function(variable) {
      var <- data[[variable]]
      var <- var[!is.na(var)]
      id <- filters_id[[variable]]
      if (inherits(x = var, what = c("numeric", "integer"))) {
        params <- find_range_step(var)
        tags$div(
          style = "position: relative;",
          tags$span(
            tags$label(variable), HTML("&nbsp;&nbsp;"), 
            na_filter(id = ns(paste0("na_", id)))
          ),
          set_slider_attr(sliderInput(
            inputId = ns(id), 
            min = params$min, 
            max = params$max, 
            width = width,
            value = params$range, 
            step = params$step,
            label = NULL
          ))
        )
      } else if (inherits(x = var, what = c("Date", "POSIXct"))) {
        range_var <- range(var)
        tags$div(
          style = "position: relative;",
          tags$span(
            tags$label(variable), HTML("&nbsp;&nbsp;"), 
            na_filter(id = ns(paste0("na_", id)))
          ),
          set_slider_attr(sliderInput(
            inputId = ns(id), 
            min = min(var), 
            max = max(var), 
            width = width,
            value = range(var), 
            label = NULL
          ))
        )
      } else {
        values <- unique(as.character(var))
        values <- values[trimws(values) != ""]
        tags$div(
          style = "position: relative;",
          class = if (length(values) > 15) "selectize-big",
          tags$span(
            tags$label(variable), HTML("&nbsp;&nbsp;"), 
            na_filter(id = ns(paste0("na_", id))) 
          ),
          selectizeInput(
            inputId = ns(id),
            choices = values, 
            selected = values, 
            label = NULL,
            multiple = TRUE, 
            width = width,
            options = list(plugins = list("remove_button"))
          )
        )
      }
    }
  )
  list(
    ui = tagList(ui),
    filters_id = filters_id,
    filters_na_id = filters_na_id
  )
}

tagSetAttributes <- function(tag, ...) {
  tag$attribs[names(list(...))] <- NULL
  tag$attribs <- c(tag$attribs, list(...))
  tag
}

set_slider_attr <- function(slider) {
  slider$children[[2]] <- tagSetAttributes(
    tag = slider$children[[2]], 
    `data-force-edges` = "true",
    `data-grid-num` = "4"
  )
  slider
}

#' @importFrom htmltools tags
#' @importFrom shinyWidgets prettySwitch
na_filter <- function(id) {
  tags$span(
    style = "position: absolute; right: 0px; margin-right: -20px;",
    prettySwitch(
      inputId = id,
      label = "NA",
      value = TRUE,
      slim = TRUE,
      status = "primary",
      inline = TRUE
    )
  )
}


#' @importFrom rlang expr sym
make_expr_filter <- function(filters, filters_na, data, data_name) {
  expressions <- lapply(
    X = names(filters),
    FUN = function(var) {
      values <- filters[[var]]
      nas <- filters_na[[var]]
      data_values <- data[[var]]
      if (!match_class(values, data_values))
        return(NULL)
      values_expr <- NULL
      if (inherits(x = values, what = c("numeric", "integer"))) {
        data_values <- find_range_step(data_values)$range
        if (!isTRUE(all.equal(values, data_values))) {
          values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2])
        }
      } else if (inherits(x = values, what = c("Date", "POSIXct"))) {
        values <- format(values)
        data_values <- range(data_values, na.rm = TRUE)
        data_values <- format(data_values)
        if (!identical(values, data_values)) {
          values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2])
        }
      } else {
        data_values <- unique(as.character(data_values))
        if (!identical(sort(values), sort(data_values))) {
          if (length(values) <= length(data_values)/2) {
            values_expr <- expr(!!sym(var) %in% !!values)
          } else {
            values_expr <- expr(!(!!sym(var) %in% !!setdiff(data_values, values)))
          }
        }
      }
      if (is.null(values_expr) & !isTRUE(nas)) {
        expr(!is.na(!!sym(var)))
      } else {
        values_expr
      }
    }
  )
  expressions <- dropNullsOrEmpty(expressions)
  expr_dplyr <- Reduce(
    f = function(x, y) expr(!!x %>% filter(!!y)), 
    x = expressions, 
    init = expr(!!sym(data_name))
  )
  expression <- Reduce(
    f = function(x, y) expr(!!x & !!y), 
    x = expressions
  )
  return(list(
    expr_dplyr = expr_dplyr,
    expr = expression
  ))
}


drop_id <- function(data) {
  data[] <- lapply(
    X = data,
    FUN = function(x) {
      if (inherits(x, c("factor", "character"))) {
        values <- unique(as.character(x))
        values <- values[trimws(values) != ""]
        if (length(values) <= 1)
          return(NULL)
        if (length(values) >= length(x) * 0.9)
          return(NULL)
        if (length(values) >= 50)
          return(NULL)
      }
      x
    }
  )
  data
}

drop_na <- function(data) {
  data[] <- lapply(
    X = data,
    FUN = function(x) {
      if (all(is.na(x)))
        return(NULL)
      x
    }
  )
  data
}


# borrowed from shiny
hasDecimals <- function (value) {
  truncatedValue <- round(value)
  return(!identical(value, truncatedValue))
}

find_range_step <- function(x) {
  max <- max(x, na.rm = TRUE)
  min <- min(x, na.rm = TRUE)
  range <- max - min
  if (range < 2 || hasDecimals(min) || hasDecimals(max)) {
    pretty_steps <- pretty(c(min, max), n = 100, high.u.bias = 1)
    n_steps <- length(pretty_steps) - 1
    list(
      range = range(pretty_steps),
      min = min(pretty_steps),
      max = max(pretty_steps),
      step = signif(digits = 10, (max(pretty_steps) - min(pretty_steps))/n_steps)
    )
  }
  else {
    list(
      range = range(x, na.rm = TRUE),
      min = min,
      max = max,
      step = 1
    )
  }
}

match_class <- function(x, y) {
  char <- c("character", "factor")
  num <- c("numeric", "integer")
  date <- c("Date", "POSIXt")
  if (inherits(x, num) & inherits(y, num))
    return(TRUE)
  if (inherits(x, char) & inherits(y, char))
    return(TRUE)
  if (inherits(x, date) & inherits(y, date))
    return(TRUE)
  return(FALSE)
}






