#' Wrapper for module chooseData, ui part
#'
#' @noRd
chooseDataUI <- function() {

  tagList(
    toggleBtnUi()
  )
}

#' Wrapper for module chooseData, server part
#'
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param data    a data.frame to use in the addin, if NULL a modal dialog is launched
#'
#' @return data chosen by the user
#' @noRd
#'
#' @importFrom shiny showModal observeEvent reactiveValues callModule
#'
chooseDataServer <- function(input, output, session, data = NULL) {

  # data <- getOption("charter.ggbuilder.data")

  # Data
  if (is.null(data)) {
    shiny::showModal(chooseDataModalUI("start"))
  }
  dataStart <- shiny::callModule(module = chooseDataModalServer,id = "start")

  dataChart <- shiny::reactiveValues(x = data)

  shiny::observeEvent(dataStart$x, {
    if (!is.null(dataStart$x)) {
      dataChart$x <- dataStart$x
      dataChart$name <- dataStart$name
    }
  }, ignoreInit = TRUE)

  return(dataChart)
}


#' Module for choosing data (ui)
#'
#' @param id Module's id
#'
#' @importFrom shiny NS modalDialog modalButton uiOutput
#' @importFrom shinyWidgets pickerInput
#' @importFrom htmltools tags
#' @importFrom utils data
#'
#' @noRd
chooseDataModalUI <- function(id) {

  # Namespace
  ns <- shiny::NS(id)

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

  shiny::modalDialog(
    title = "Choose a dataset",
    footer = htmltools::tags$div(
      shiny::actionButton(inputId = ns("validata"), label = "Choose", class = "btn-primary", `data-dismiss` = "modal"),
      shiny::modalButton(label = "Dismiss")
    ),
    shinyWidgets::pickerInput(
      inputId = ns("data"),
      label = "Choose a data.frame :",
      choices = dfs, width = "100%",
      options = list(title = "List of data.frame..."),
      choicesOpt = list(subtext = info_dfs)
    ),
    shiny::uiOutput(outputId = ns("col_chooser_ui")),
    shiny::uiOutput(outputId = ns("alert_no_var"))
  )
}



#' Module for choosing data (server)
#'
#' @param input    standard \code{shiny} input
#' @param output   standard \code{shiny} output
#' @param session  standard \code{shiny} session
#'
#' @noRd
#'
#' @importFrom shinyWidgets pickerInput
#' @importFrom htmltools tags HTML tagList
#' @importFrom shiny renderUI observeEvent req
#'
chooseDataModalServer <- function(input, output, session) {

  # Namespace
  ns <- session$ns

  dataChoosen <- reactiveValues(x = NULL)


  output$col_chooser_ui <- renderUI({

    req(input$data)

    dat <- get_df(input$data)
    # dat <- as.data.table(dat)

    res_col_type <- unlist(lapply(dat, col_type))

    htmltools::tagList(
      # shinyWidgets::multiInput(
      #   inputId = ns("col_chooser"), label = "Validate choosen variable :",
      #   choiceNames = badgeType(col_name = names(res_col_type), col_type = unname(res_col_type)),
      #   choiceValues = names(res_col_type),
      #   selected = names(res_col_type)[unname(res_col_type) != "id"]
      # ),
      shinyWidgets::pickerInput(
        inputId = ns("col_chooser"),
        label = "Validate choosen variable :",
        choices = names(res_col_type), multiple = TRUE, width = "100%",
        selected = names(res_col_type)[unname(res_col_type) != "id"],
        options = list(
          `actions-box` = TRUE, `multiple-separator` = " ",
          `selected-text-format`= "count > 4",
          `count-selected-text` = "{0} variables choosed (on a total of {1})"
        ),
        choicesOpt = list(
          content = badgeType(col_name = names(res_col_type), col_type = unname(res_col_type))
        )
      ),
      htmltools::tags$em("Legend :"),
      htmltools::HTML(paste(
        badgeType(col_name = c("categorical", "continuous", "time", "id"),
                   col_type = c("categorical", "continuous", "time", "id")),
        collapse = ", "
      ))
    )

  })


  toggleBtnServer(session, inputId = ns("validata"), type = "disable")
  output$alert_no_var <- shiny::renderUI({
    shiny::req(input$data)
    if (length(input$col_chooser) < 1) {

      toggleBtnServer(session, inputId = ns("validata"), type = "disable")
      htmltools::tagList(
        htmltools::tags$br(),
        htmltools::tags$div(class = "alert alert-warning", tags$b("Warning !"), "no variable selected...")
      )

    } else {

      toggleBtnServer(session, inputId = ns("validata"), type = "enable")
      NULL

    }
  })

  shiny::observeEvent(input$validata, {
    dat <- get_df(input$data)
    dat <- as.data.frame(dat)
    dat <- dat[, input$col_chooser, drop = FALSE]
    dataChoosen$x <- dat
    dataChoosen$name <- input$data
  })


  return(dataChoosen)
}

