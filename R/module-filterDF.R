
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
#' @example examples/filterDF.R
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
#' @param label_nrow Text to display before the number of rows of filtered data / source data.
#' @param drop_ids Drop columns containing more than 90\% of unique values, or than 50 distinct values.
#' @param picker Use  \code{\link[shinyWidgets:pickerInput]{shinyWidgets::pickerInput}}
#'  instead of  \code{\link[shiny:selectInput]{shiny::selectizeInput}} (default).
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
                     data_name = reactive("data"),
                     label_nrow = "Number of rows:",
                     drop_ids = TRUE,
                     picker = FALSE) {
  
  ns <- session$ns
  jns <- function(x) paste0("#", ns(x))
  
  output$nrow <- renderUI({
    if (!is.null(label_nrow)) {
      tags$p(label_nrow, tags$b(nrow(data_filtered()) , "/", nrow(data_table())))
    }
  })
  
  rv_filters <- reactiveValues(mapping = NULL, mapping_na = NULL)
  rv_code <- reactiveValues(expr = NULL, dplyr = NULL)
  
  observe({
    data <- data_table()
    vars <- data_vars()
    # req(nrow(data) > 0)
    removeUI(selector = jns("filters_inputs"), immediate = TRUE)
    filters <- create_filters(
      data = data, vars = vars, 
      drop_ids = drop_ids, picker = picker
    )
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
        # req(input[[x]])
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
#' @importFrom shinyWidgets pickerInput pickerOptions
create_filters <- function(data, vars = NULL,
                           drop_ids = TRUE,
                           picker = FALSE,
                           width = "100%", session = getDefaultReactiveDomain()) {
  ns <- session$ns
  data <- drop_na(data)
  if (isTRUE(drop_ids)) {
    data <- drop_id(data)
  }
  data <- dropListColumns(data)
  if (is.null(vars)) {
    vars <- names(data)
  } else {
    vars <- intersect(names(data), vars)
  }
  filters_id <- paste0("filter_", makeId(vars))
  filters_id <- setNames(as.list(filters_id), vars)
  filters_na_id <- setNames(as.list(paste0("na_", filters_id)), vars)
  ui <- lapply(
    X = vars,
    FUN = function(variable) {
      var <- data[[variable]]
      any_na <- anyNA(var)
      var <- var[!is.na(var)]
      id <- filters_id[[variable]]
      tag_label <- if (any_na) {
        tags$span(
          tags$label(variable), HTML("&nbsp;&nbsp;"), 
          na_filter(id = ns(paste0("na_", id)))
        )
      } else {
        tags$span(tags$label(variable), HTML("&nbsp;&nbsp;"))
      }
      if (inherits(x = var, what = c("numeric", "integer"))) {
        params <- find_range_step(var)
        tags$div(
          style = "position: relative;",
          tag_label,
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
          tag_label,
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
        values <- tryCatch(values[trimws(values) != ""], error = function(e){
          Encoding(values[!validEnc(values)]) <- "unknown"
          values
        })
        if (isTRUE(picker)) {
          tags$div(
            style = "position: relative;",
            tag_label,
            pickerInput(
              inputId = ns(id),
              choices = values, 
              selected = values, 
              label = NULL,
              multiple = TRUE, 
              width = width, 
              options = pickerOptions(
                actionsBox = TRUE, 
                selectedTextFormat = "count", 
                liveSearch = TRUE
              )
            )
          )
        } else {
          tags$div(
            style = "position: relative;",
            class = if (length(values) > 15) "selectize-big",
            tag_label,
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
      if (!is.null(values) & !match_class(values, data_values))
        return(NULL)
      values_expr <- NULL
      if (inherits(x = values, what = c("numeric", "integer"))) {
        data_range <- find_range_step(data_values)$range
        if (!isTRUE(all.equal(values, data_range))) {
          if (isTRUE(nas)) {
            if (anyNA(data_values)) {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2] | is.na(!!sym(var)))
            } else {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2])
            }
          } else {
            if (anyNA(data_values)) {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2] & !is.na(!!sym(var)))
            } else {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2])
            }
          }
        }
      } else if (inherits(x = values, what = c("Date", "POSIXct"))) {
        values <- format(values)
        data_range <- range(data_values, na.rm = TRUE)
        data_range <- format(data_range)
        if (!identical(values, data_range)) {
          if (isTRUE(nas)) {
            if (anyNA(data_values)) {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2] | is.na(!!sym(var)))
            } else {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2])
            }
          } else {
            if (anyNA(data_values)) {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2] & !is.na(!!sym(var)))
            } else {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2])
            }
          }
        }
      } else {
        data_values <- unique(as.character(data_values))
        if (!identical(sort(values), sort(data_values))) {
          if (length(values) == 0) {
            if (isTRUE(nas)) {
              values_expr <- expr(is.na(!!sym(var)))
            } else {
              values_expr <- expr(!(!!sym(var) %in% !!data_values[!is.na(data_values)]) & !is.na(!!sym(var)))
            }
          } else {
            if (length(values) <= length(data_values)/2) {
              if (isTRUE(nas)) {
                if (anyNA(data_values)) {
                  values_expr <- expr(!!sym(var) %in% !!values | is.na(!!sym(var)))
                } else {
                  values_expr <- expr(!!sym(var) %in% !!values)
                }
              } else {
                values_expr <- expr(!!sym(var) %in% !!values)
              }
            } else {
              if (isTRUE(nas)) {
                if (anyNA(data_values)) {
                  values_expr <- expr(!(!!sym(var) %in% !!setdiff(data_values[!is.na(data_values)], values)) | is.na(!!sym(var)))
                } else {
                  values_expr <- expr(!(!!sym(var) %in% !!setdiff(data_values[!is.na(data_values)], values)))
                }
              } else {
                if (anyNA(data_values)) {
                  values_expr <- expr(!(!!sym(var) %in% !!setdiff(data_values[!is.na(data_values)], values)) & !is.na(!!sym(var)))
                } else {
                  values_expr <- expr(!(!!sym(var) %in% !!setdiff(data_values[!is.na(data_values)], values)))
                }
              }
            }
          }
        }
      }
      if (is.null(values_expr) & !isTRUE(nas) & anyNA(data_values)) {
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
        values <- tryCatch(values[trimws(values) != ""], error = function(e){
          Encoding(values[!validEnc(values)]) <- "unknown"
          values
        })
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






