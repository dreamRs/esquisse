
#' @title Deprecated functions
#' 
#' @note The following functions are deprecated and will be removed in next release:
#'  * `esquisserUI` / `esquisserServer`: replaced by `esquisse_ui` / `esquisse_server`
#'  * `filterDF_UI` / `filterDF`: moved to package [datamods](https://github.com/dreamRs/datamods)
#'  * `chooseDataUI` / `chooseDataServer`: moved to package [datamods](https://github.com/dreamRs/datamods)
#'  * `coerceUI` / `coerceServer`: moved to package [datamods](https://github.com/dreamRs/datamods)
#'  
#' @name esquisse-deprecated
#'  
NULL


# filterDF ----------------------------------------------------------------


#' @title Shiny module to interactively filter a \code{data.frame}
#' 
#' @description DEPRECATED, please see package \href{https://github.com/dreamRs/datamods}{datamods} for similar features.
#'
#' @param id Module id. See \code{\link[shiny]{callModule}}.
#' @param show_nrow Show number of filtered rows and total.
#'
#' @return A `list` with 2 elements :
#'   * \strong{data_filtered} : \code{\link[shiny]{reactive}} function returning data filtered.
#'   * \strong{code} : \code{\link[shiny]{reactiveValues}} with 2 slots :
#'    \code{expr} (raw expression to filter data) and \code{dplyr} (code with dplyr pipeline).
#'  
#' @export
#' 
#' @name module-filterDF
#' 
#' @importFrom htmltools tagList singleton tags
#' @importFrom shiny NS uiOutput
#'
filterDF_UI <- function(id, show_nrow = TRUE) {
  
  .Deprecated(new = "datamods::filter_data_ui / datamods::filter_data_server", package = "esquisse", old = "filterDF_UI / filterDF")
  
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
#' @param drop_ids Drop columns containing more than 90% of unique values, or than 50 distinct values.
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




# Utils filterDF ----------------------------------------------------------


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




# chooseData --------------------------------------------------------------



#' @title Module for choosing data.frame
#'
#' @description DEPRECATED, please see package \href{https://github.com/dreamRs/datamods}{datamods} for similar features.
#'
#' @param id Module's id.
#' @param label Label for button, passed to \code{\link[shiny:actionButton]{actionButton}}.
#' @param icon Icon to appears on the button, passed to \code{\link[shiny:actionButton]{actionButton}}.
#' @param width Width of button, passed to \code{\link[shiny:actionButton]{actionButton}}.
#' @param ... Other arguments passed to \code{\link[shiny:actionButton]{actionButton}}
#'
#' @return a \code{\link[shiny]{reactiveValues}} containing the data selected under slot \code{data}
#' and the name of the selected \code{data.frame} under slot \code{name}.
#' @export
#'
#' @name module-chooseData
#'
#' @importFrom htmltools tagList tags singleton
#' @importFrom shiny NS actionButton icon
#'
chooseDataUI <- function(id,
                         label = "Data",
                         icon = "database",
                         width = "100%",
                         ...) {
  .Deprecated(new = "datamods::import_ui / datamods::import_server", package = "esquisse", old = "chooseDataUI / chooseDataServer")
  
  ns <- NS(id)
  if (is.character(icon))
    icon <- icon(icon)
  
  tagList(
    html_dependency_dragula(),
    html_dependency_esquisse(),
    actionButton(
      inputId = ns("changeData"),
      label = label,
      icon = icon,
      width = width,
      ...
    )
  )
}

#' @param input,output,session standards \code{shiny} server arguments.
#' @param dataModule Data module to use, choose between \code{"GlobalEnv"}
#'  (select ad \code{data.frame} from Global environment)
#'  or \code{"ImportFile"} (import an external file supported by \code{\link[rio]{import}}).
#' @param data A \code{data.frame} to use by default.
#' @param name Character, object's name to use for \code{data}.
#' @param selectVars Display module to select variables, \code{TRUE} by default.
#' @param selectedTypes Type of variables selected by default in select variables module.
#'  Possible types are \code{"discrete"}, \code{"time"}, \code{"continuous"} and \code{"id"},
#'  by default \code{"id"} is discarded.
#' @param coerceVars Display module to coerce variables between different class, \code{TRUE} by default.
#' @param launchOnStart Opens modal window when the application starts.
#' @param size Size for the modal window.
#'
#'
#' @export
#'
#' @rdname module-chooseData
#'
#' @importFrom shiny showModal modalDialog observeEvent reactiveValues callModule observe icon
#' @importFrom htmltools tags HTML
chooseDataServer <- function(input,
                             output,
                             session,
                             dataModule = c("GlobalEnv", "ImportFile"),
                             data = NULL,
                             name = NULL,
                             selectVars = TRUE,
                             selectedTypes = c("continuous", "discrete", "time"),
                             coerceVars = FALSE,
                             launchOnStart = TRUE,
                             size = "m") {
  
  dataModule <- match.arg(dataModule)
  datModUI <- switch(
    dataModule,
    "GlobalEnv" = dataGlobalEnvUI,
    "ImportFile" = dataImportFileUI
  )
  datModServer <- switch(
    dataModule,
    "GlobalEnv" = dataGlobalEnvServer,
    "ImportFile" = dataImportFileServer
  )
  
  ns <- session$ns
  return_data <- reactiveValues(data = data, name = name)
  
  if (isTRUE(launchOnStart)) {
    showModal(modalDialog(tagList(
      tags$button(
        icon("close"),
        class = "btn btn-link pull-right",
        `data-dismiss` = "modal"
      ),
      datModUI(
        id = ns("chooseData"),
        selectVars = selectVars,
        coerceVars = coerceVars
      )
    ), size = size, fade = FALSE, footer = NULL))
  }
  
  observeEvent(input$changeData, {
    showModal(modalDialog(tagList(
      tags$button(
        icon("close"),
        class = "btn btn-link pull-right",
        `data-dismiss` = "modal"
      ),
      datModUI(
        id = ns("chooseData"),
        selectVars = selectVars,
        coerceVars = coerceVars
      )
    ), size = size, fade = FALSE, footer = NULL))
  })
  
  return_data <- callModule(
    module = datModServer,
    id = "chooseData",
    data = data,
    name = name,
    selectedTypes = selectedTypes
  )
  
  return(return_data)
}




# Coerce ------------------------------------------------------------------



#' @title Coerce data.frame's columns module
#' 
#' @description DEPRECATED, please see package \href{https://github.com/dreamRs/datamods}{datamods} for similar features.
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
coerceUI <- function(id) {
  .Deprecated(new = "datamods::import_ui / datamods::import_server", package = "esquisse", old = "coerceUI / coerceServer")
  ns <- NS(id)
  fluidRow(
    tags$style(
      ".col-coerce {padding-right: 5px; padding-left: 5px;}"
    ),
    html_dependency_esquisse(),
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
          shiny::showNotification(
            ui = conditionMessage(e),
            type = "error",
            session = session,
            id = paste("esquisse", sample.int(1e6, 1), sep = "-")
          )
        }
      ),
      warning = function(w) {
        shiny::showNotification(
          ui = conditionMessage(w),
          type = "warning",
          session = session,
          id = paste("esquisse", sample.int(1e6, 1), sep = "-")
        )
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




# dataGlobalEnv -----------------------------------------------------------

#' Module to choose a \code{data.frame} in \code{GlobalEnv}
#'
#' @param id Module id. See \code{\link[shiny]{callModule}}.
#' @param dismissOnValidate Dismiss modal when user validate data, \code{TRUE} by default.
#' @param selectVars Display module to select variables, \code{TRUE} by default.
#' @param coerceVars Display module to coerce variables between different class, \code{TRUE} by default.
#'
#' @noRd
#'
#' @name module-data-globalenv
#'
#' @importFrom htmltools tagList tags HTML
#' @importFrom shiny NS actionButton icon
#' @importFrom shinyWidgets pickerInput
#'
dataGlobalEnvUI <- function(id, dismissOnValidate = TRUE, selectVars = TRUE, coerceVars = TRUE) {
  
  .Deprecated(new = "datamods::import_ui / datamods::import_server", package = "esquisse", old = "dataGlobalEnvUI / dataGlobalEnvServer")
  
  ns <- NS(id)
  
  # List of data.frame
  dfs <- search_obj(what = "data.frame")
  if (is.null(dfs)) {
    dfs <- data(package = "ggplot2", envir = environment())$results[, "Item"]
  }
  
  info_dfs <- lapply(
    X = dfs,
    FUN = function(x) {
      tmp <- get_df(x)
      sprintf("%d obs. of  %d variables", nrow(tmp), ncol(tmp))
    }
  )
  info_dfs <- unlist(info_dfs)
  
  tagList(
    html_dependency_esquisse(),
    tags$script(
      sprintf("Shiny.onInputChange('%s', %f);", ns("dataGlobalEnv"), as.numeric(Sys.time()))
    ),
    tags$h2("Select a dataset"),
    pickerInput(
      inputId = ns("data"),
      label = "Choose a data.frame :",
      choices = dfs, width = "100%",
      options = list(title = "List of data.frame..."),
      choicesOpt = list(subtext = info_dfs)
    ),
    
    tags$div(
      id = ns("placeholder-result-import"),
      tags$div(
        id = ns("result-import"), class = "alert alert-info",
        tags$b("No data selected"), "Use a data.frame from user environment"
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
#' @rdname module-data-globalenv
#'
#' @importFrom shiny reactiveValues observeEvent req removeUI insertUI callModule
dataGlobalEnvServer <- function(input, output, session, data = NULL, name = NULL,
                                selectedTypes = c("continuous", "discrete", "time")) {
  
  ns <- session$ns
  jns <- function(x) paste0("#", ns(x))
  
  imported_data <- reactiveValues(data = data, name = name)
  tmp_name <- reactiveValues(name = name)
  select_data <- reactiveValues(data = NULL, name = NULL, timestamp = Sys.time())
  coerce_data <- reactiveValues(data = NULL, name = NULL, timestamp = Sys.time())
  
  observeEvent(input$dataGlobalEnv, {
    imported_data$data <- NULL
    imported_data$name <- NULL
  })
  
  observeEvent(input$data, {
    req(input$data)
    imported <- try(get_df(input$data), silent = TRUE)
    if ("try-error" %in% class(imported) || NROW(imported) < 1) {
      toggleInput(inputId = ns("validate"), enable = FALSE)
      removeUI(selector = jns("result-import"))
      insertUI(
        selector = jns("placeholder-result-import"),
        ui = tags$div(
          id = ns("result-import"), class = "alert alert-danger",
          tags$b("Ooops"), "Something went wrong"
        )
      )
      select_data$data <- NULL
      coerce_data$data <- NULL
      tmp_name$name <- NULL
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
      select_data$data <- imported
      coerce_data$data <- imported
      tmp_name$name <- input$data
      select_data$timestamp <- Sys.time()
    }
  }, ignoreInit = TRUE)
  
  sv <- callModule(
    module = selectVarsServer,
    id = "selected",
    data = select_data,
    selectedTypes = selectedTypes
  )
  
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
    imported_data$data <- dat
    imported_data$name <- tmp_name$name
  })
  
  return(imported_data)
}





# dataImportFile ----------------------------------------------------------


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
  
  .Deprecated(new = "datamods::import_ui / datamods::import_server", package = "esquisse", old = "dataImportFileUI / dataImportFileServer")
  
  ns <- NS(id)
  
  tagList(
    html_dependency_esquisse(),
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
dataImportFileServer <- function(input, output, session, data = NULL, name = NULL,
                                 selectedTypes = c("continuous", "discrete", "time")) {
  
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
  
  
  sv <- callModule(
    module = selectVarsServer,
    id = "selected",
    data = select_data,
    selectedTypes = selectedTypes
  )
  
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






# Select ------------------------------------------------------------------

#' @importFrom htmltools tagList tags HTML
#' @importFrom shiny NS icon
#' @importFrom shinyWidgets pickerInput
selectVarsUI <- function(id) {
  ns <- NS(id)
  tagList(
    html_dependency_esquisse(),
    tags$script(
      sprintf("Shiny.onInputChange('%s', %f);", ns("launchSelectVars"), as.numeric(Sys.time()))
    ),
    tags$style(HTML(paste(
      paste0("#", ns("col_chooser-container")),
      ".bootstrap-select .dropdown-menu li a span.text {width: 96%;}"
    ))),
    tags$div(
      id = ns("col_chooser-container"),
      tags$label(
        class = "control-label",
        style = "width: 100%;",
        "Select variables to keep :",
        tags$a(
          id = ns("help-select-vars"), style = "float: right;",
          style = "color: steelblue;", icon("info-circle", class = "fa-lg"),
          `data-toggle` = "popover", `data-trigger` = "hover", `data-animation` = "false",
          `data-container` = "body", tabindex = "0", role = "button",
          `data-content` = "Select the variables you want to use to create graphs",
          tags$script(sprintf("$('#%s').popover();", ns("help-select-vars")))
        )
      ),
      pickerInput(
        inputId = ns("col_chooser"),
        label = NULL,
        choices = "No data", multiple = TRUE, width = "100%",
        selected = NULL,
        options = list(
          `actions-box` = TRUE, `multiple-separator` = " ",
          `selected-text-format`= "count > 0",
          `count-selected-text` = "{0} variables chosen (on a total of {1})"
        )
      )
    ),
    tags$em("Legend :"),
    HTML(paste(
      doRenderTags(
        badgeType(col_name = c("discrete", "continuous", "time", "id"),
                  col_type = c("discrete", "continuous", "time", "id"))
      ),
      collapse = ", "
    ))
  )
}


#' @importFrom htmltools doRenderTags
#' @importFrom shiny reactiveValuesToList observeEvent reactiveValues
#' @importFrom shinyWidgets updatePickerInput
selectVarsServer <- function(input, output, session, data = list(),
                             selectedTypes = c("continuous", "discrete", "time")) {
  
  ns <- session$ns
  
  observeEvent(input$launchSelectVars, {
    toggleInput(inputId = ns("col_chooser"), enable = FALSE)
  })
  
  observeEvent(reactiveValuesToList(data), {
    if (!is.null(data$data) && is.data.frame(data$data)) {
      toggleInput(inputId = ns("col_chooser"), enable = TRUE)
    } else {
      toggleInput(inputId = ns("col_chooser"), enable = FALSE)
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(reactiveValuesToList(data), {
    if (!is.null(data$data) && is.data.frame(data$data)) {
      res_col_type <- unlist(lapply(data$data, col_type))
      updatePickerInput(
        session = session,
        inputId = "col_chooser",
        choices =  names(res_col_type),
        selected = names(res_col_type)[unname(res_col_type) %in% selectedTypes],
        choicesOpt = list(
          content = paste0(
            unlist(lapply(
              X = badgeType(col_name = names(res_col_type), col_type = unname(res_col_type)),
              FUN = doRenderTags
            )),
            "<span style='float: right; margin-right: 15px; white-space: pre;'>",
            "Class: ",
            "<span style='display: inline-block; width: 65px; text-align: right;'>",
            unlist(lapply(data$data, function(x) class(x)[1]), use.names = FALSE),
            "</span>",
            " |   ",
            "Missing values: ",
            "<span style='display: inline-block; width: 30px; text-align: right;'>",
            unlist(lapply(data$data, function(x) sum(is.na(x))), use.names = FALSE),
            "</span>",
            "</span>"
          )
        )
      )
    }
  }, ignoreNULL = FALSE)
  
  res <- reactiveValues(selected_vars = NULL)
  
  observeEvent(input$col_chooser, {
    res$selected_vars <- input$col_chooser
  }, ignoreNULL = FALSE)
  
  return(res)
}




# run module --------------------------------------------------------------



#' @title Run module example
#' 
#' @description DEPRECATED, please see package \href{https://github.com/dreamRs/datamods}{datamods} for similar features.
#'
#' @param module Module for which to see a demo.
#'
#' @export
#' 
#' @importFrom shiny shinyAppDir
#'
run_module <- function(module = c("filterDF", "chooseData", "chooseData2", "coerce")) {
  .Deprecated(new = "See package datamods", package = "esquisse", old = "run_module")
  module <- match.arg(module)
  path <- file.path("modules-examples", module)
  shiny::shinyAppDir(
    appDir = system.file(path, package = "esquisse", mustWork=TRUE), 
    options = list(display.mode = "showcase")
  )
}


