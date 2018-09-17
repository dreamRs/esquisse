
#' @title Module for choosing data.frame
#' 
#' @description Module for choosing data.frame from
#' user environment and select variable to use.
#'
#' @param id Module's id.
#'
#' @return a \code{\link[shiny]{reactiveValues}} containing the data selected under slot \code{data}
#' and the name of the selected \code{data.frame} under slot \code{name}.
#' @export
#' 
#' @name chooseData-module
#' 
#' @importFrom htmltools tagList tags
#' @importFrom shiny NS actionButton icon singleton
#'
#' @examples
#' 
#' if (interactive()) {
#' 
#' 
#' library(shiny)
#' library(esquisse)
#' 
#' ui <- fluidPage(
#'   tags$h2("Choose data module"),
#'   fluidRow(
#'     column(
#'       width = 4,
#'       tags$h4("Default"),
#'       chooseDataUI(id = "choose1"),
#'       verbatimTextOutput(outputId = "res1")
#'     ),
#'     column(
#'       width = 4,
#'       tags$h4("No var selection"),
#'       chooseDataUI(id = "choose2"),
#'       verbatimTextOutput(outputId = "res2")
#'     ),
#'     column(
#'       width = 4,
#'       tags$h4("Default data on start"),
#'       chooseDataUI(id = "choose3"),
#'       verbatimTextOutput(outputId = "res3")
#'     )
#'   )
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   res_dat1 <- callModule(
#'     chooseDataServer, id = "choose1",
#'     launchOnStart = FALSE
#'   )
#'   output$res1 <- renderPrint({
#'     str(reactiveValuesToList(res_dat1))
#'   })
#'   
#'   res_dat2 <- callModule(
#'     chooseDataServer, id = "choose2", selectVars = FALSE,
#'     launchOnStart = FALSE
#'   )
#'   output$res2 <- renderPrint({
#'     str(reactiveValuesToList(res_dat2))
#'   })
#'   
#'   res_dat3 <- callModule(
#'     chooseDataServer, id = "choose3", data = iris,
#'     launchOnStart = FALSE
#'   )
#'   output$res3 <- renderPrint({
#'     str(reactiveValuesToList(res_dat3))
#'   })
#'   
#' }
#' 
#' shinyApp(ui, server)
#' 
#' 
#' }
#' 
chooseDataUI <- function(id) {
  
  ns <- NS(id)

  tagList(
    singleton(
      tags$link(rel="stylesheet", type="text/css",
                href="esquisse/styles-dad.css")
    ),
    toggleBtnUi(),
    actionButton(
      inputId = ns("changeData"), label = "Choose data",
      icon = icon("database"), width = "100%"
    )
  )
}

#' @param input standard \code{shiny} input.
#' @param output standard \code{shiny} output.
#' @param session standard \code{shiny} session.
#' @param data a data.frame to use by default.
#' @param name character, the name of \code{data}.
#' @param selectVars logical, display menu to select vars to use in selected \code{data.frame}.
#' @param coerceVars logical, display menu to coerce a variable in a different class.
#' @param launchOnStart launch modal when application is launched.
#' @param defaultData a character vector of \code{data.frame}s to choose along if
#' there is no \code{data.frame}s in Global environment. By default, \code{data.frame}s
#' from \code{ggplot2} are used.
#' 
#' @export
#'
#' @rdname chooseData-module
#'
#' @importFrom shiny showModal observeEvent reactiveValues callModule observe
#' @importFrom htmltools tags HTML
chooseDataServer <- function(input, output, session, data = NULL, name = NULL, 
                             selectVars = TRUE, coerceVars = FALSE, 
                             launchOnStart = TRUE, defaultData = NULL) {

  ns <- session$ns
  
  return_data <- reactiveValues(data = data, name = name)
  tmp_data <- reactiveValues(data = data, name = name)
  
  if (launchOnStart) {
    showModal(chooseDataModal(
      ns = ns, 
      defaultData = defaultData, 
      selectVars = selectVars,
      coerceVars = coerceVars
    ))
  }

  # Data
  observeEvent(input$changeData, {
    tmp_data$data <- NULL
    tmp_data$name <- NULL
    showModal(chooseDataModal(
      ns = ns, 
      defaultData = defaultData, 
      selectVars = selectVars, 
      coerceVars = coerceVars
    ))
  }, ignoreInit = TRUE)
  
  
  
  output$col_chooser_ui <- renderUI({
    req(input$data)
    dat <- get_df(input$data)
    res_col_type <- unlist(lapply(dat, col_type))
    tagList(
      pickerInput(
        inputId = ns("col_chooser"),
        label = "Validate chosen variable :",
        choices = names(res_col_type), multiple = TRUE, width = "100%",
        selected = names(res_col_type)[unname(res_col_type) != "id"],
        options = list(
          `actions-box` = TRUE, `multiple-separator` = " ",
          `selected-text-format`= "count > 4",
          `count-selected-text` = "{0} variables chosen (on a total of {1})"
        ),
        choicesOpt = list(
          content = unlist(lapply(
            X = badgeType(col_name = names(res_col_type), col_type = unname(res_col_type)),
            FUN = htmltools::doRenderTags
          ))
        )
      ),
      tags$em("Legend :"),
      HTML(paste(
        doRenderTags(
          badgeType(col_name = c("categorical", "continuous", "time", "id"),
                    col_type = c("categorical", "continuous", "time", "id"))
        ),
        collapse = ", "
      ))
    )
  })
  
  toggleBtnServer(session, inputId = ns("validata"), type = "disable")
  output$alert_no_var <- shiny::renderUI({
    shiny::req(input$data)
    if (length(input$col_chooser) < 1) {
      toggleBtnServer(session, inputId = ns("validata"), type = "disable")
      tagList(
        tags$br(),
        tags$div(
          class = "alert alert-warning",
          tags$b("Warning !"), "no variable selected..."
        )
      )
    } else {
      toggleBtnServer(session, inputId = ns("validata"), type = "enable")
      NULL
    }
  })
  
  output$coerce_ui <- renderUI({
    req(input$data)
    tagList(
      tags$br(),
      tags$div(coerceUI(id = ns("coerce")), style = "margin: 10px;")
    )
  })
  
  observe({
    req(input$data); req(input$col_chooser)
    dat <- get_df(input$data)
    ## --->>> TODO SF <<<--- ##
    # if (inherits(dat, what = "sf")) {
    #   var_chosen <- c(input$col_chooser, attr(dat, "sf_column"))
    # } else {
      dat <- as.data.frame(dat)
      var_chosen <- input$col_chooser
    # }
    if (selectVars & all(var_chosen %in% names(dat))) {
      dat <- dat[, input$col_chooser, drop = FALSE]
    }
    tmp_data$data <- dat
    tmp_data$name <- input$data
    tmp_data$timestamp <- Sys.time()
  })
  
  coerce_data <- callModule(module = coerceServer, id = "coerce", data = tmp_data)
  
  observeEvent(input$validata, {
    if (!is.null(coerce_data$data)) {
      return_data$data <- coerce_data$data
    } else {
      return_data$data <- tmp_data$data
    }
    return_data$name <- tmp_data$name
  })

  return(return_data)
}



#' @importFrom shiny NS modalDialog modalButton uiOutput actionButton
#' @importFrom shinyWidgets pickerInput
#' @importFrom htmltools tags
#' @importFrom utils data
chooseDataModal <- function(ns, defaultData = NULL, selectVars = TRUE, coerceVars = FALSE) {

  if (is.null(defaultData)) {
    defaultData <- data(package = "ggplot2", envir = environment())$results[, "Item"]
  }

  # List of data.frame
  dfs <- search_obj(what = "data.frame")
  if (is.null(dfs)) {
    dfs <- defaultData
  }

  info_dfs <- lapply(
    X = dfs,
    FUN = function(x) {
      tmp <- get_df(x)
      sprintf("%d obs. of  %d variables", nrow(tmp), ncol(tmp))
    }
  )
  info_dfs <- unlist(info_dfs)

  modalDialog(
    title = "Choose a dataset", fade = FALSE,
    footer = htmltools::tags$div(
      actionButton(
        inputId = ns("validata"), label = "Choose", 
        class = "btn-primary", `data-dismiss` = "modal"
      ),
      modalButton(label = "Dismiss")
    ),
    pickerInput(
      inputId = ns("data"),
      label = "Choose a data.frame :",
      choices = dfs, width = "100%",
      options = list(title = "List of data.frame..."),
      choicesOpt = list(subtext = info_dfs)
    ),
    if (selectVars) uiOutput(outputId = ns("col_chooser_ui")),
    if (selectVars) uiOutput(outputId = ns("alert_no_var")),
    if (coerceVars) uiOutput(outputId = ns("coerce_ui")), 
    tags$br()
  )
}


