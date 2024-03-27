
#' Controls for showing code
#'
#'
#' @param id Module ID.
#'
#' @noRd
#'
controls_code_ui <- function(id, insert_code = FALSE) {

  ns <- NS(id)

  tagList(
    tags$b(i18n("Code:")),
    tags$button(
      class = "btn btn-link btn-xs pull-right float-end btn-copy-code p-0",
      ph("copy"), i18n("Copy to clipboard"),
      `data-clipboard-target` = paste0("#", ns("code"))
    ),
    tags$div(class = "clearfix"),
    tags$script("$(function() {new ClipboardJS('.btn-copy-code');});"),
    uiOutput(outputId = ns("code")),
    # tags$textarea(id = ns("holderCode"), style = "display: none;"),
    if (insert_code) {
      actionLink(
        inputId = ns("insert_code"),
        label = tagList(ph("arrow-circle-left"), i18n("Insert code in script"))
      )
    },
    tags$br()
  )
}



controls_code_server <- function(id, ggplot_rv, output_filter, data_name)  {
  moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      observeEvent(input$insert_code, {
        context <- rstudioapi::getSourceEditorContext()
        code <- ggplot_rv$code
        expr <- output_filter$expr()
        if (!is.null(expr) & !isTRUE(input$disable_filters)) {
          code_dplyr <- deparse2(output_filter$code())
          code_dplyr <- paste(code_dplyr, collapse = "\n")
          nm_dat <- data_name()
          code <- gsub(x = code, replacement = " ggplot()", pattern = sprintf("ggplot(%s)", nm_dat), fixed = TRUE)
          code <- paste(code_dplyr, code, sep = " %>%\n")
          if (input$insert_code == 1) {
            code <- paste("library(dplyr)\nlibrary(ggplot2)", code, sep = "\n\n")
          }
        } else {
          if (input$insert_code == 1) {
            code <- paste("library(ggplot2)", code, sep = "\n\n")
          }
        }
        rstudioapi::insertText(text = paste0("\n", code, "\n"), id = context$id)
      })

      output$code <- renderUI({
        code <- style_code(ggplot_rv$code)
        expr <- output_filter$expr()
        if (!is.null(expr) & !isTRUE(input$disable_filters)) {
          code_dplyr <- deparse2(output_filter$code())
          nm_dat <- data_name()
          code <- gsub(x = code, replacement = " ggplot()", pattern = sprintf("ggplot(%s)", nm_dat), fixed = TRUE)
          code <- paste(code_dplyr, code, sep = " %>%\n")
        }
        HTML(downlit::highlight(code, pre_class = "esquisse-code", code = TRUE, classes = downlit::classes_pandoc()))
      })

    }
  )
}


