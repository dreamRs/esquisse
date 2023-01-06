

select_aes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("ui_aesthetics"))
  )
}

select_aes_server <- function(id,
                              data_r = reactive(NULL),
                              default_aes = c("fill", "color", "size", "group", "facet"),
                              input_aes = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      res_rv <- reactiveValues(aes = list())
      
      # Generate drag-and-drop input
      output$ui_aesthetics <- renderUI({
        if (is.reactive(default_aes)) {
          aesthetics <- default_aes()
        } else {
          if (is.null(input_aes())) {
            aesthetics <- default_aes
          } else {
            aesthetics <- input_aes()
          }
        }
        data <- isolate(data_r())
        if (!is.null(data)) {
          var_choices <- get_col_names(data)
          choiceValues <- var_choices
          choiceNames <- badgeType(
            col_name = var_choices,
            col_type = col_type(data[, var_choices, drop = TRUE])
          )
          selected <- dropNulls(isolate(input$dragvars$target))
        } else {
          choiceValues <- ""
          choiceNames <- ""
          selected <- NULL
        }
        dragulaInput(
          inputId = ns("dragvars"),
          sourceLabel = "Variables",
          targetsLabels = c("X", "Y", aesthetics),
          targetsIds = c("xvar", "yvar", aesthetics),
          choiceValues = choiceValues,
          choiceNames = choiceNames,
          selected = selected,
          badge = FALSE,
          width = "100%",
          height = "70px",
          replace = TRUE
        )
      })
      
      # Update drag-and-drop input when data changes
      observeEvent(data_r(), {
        data <- data_r()
        if (is.null(data)) {
          updateDragulaInput(
            session = session,
            inputId = "dragvars",
            status = NULL,
            choices = character(0),
            badge = FALSE
          )
        } else {
          var_choices <- get_col_names(data)
          updateDragulaInput(
            session = session,
            inputId = "dragvars",
            status = NULL,
            choiceValues = var_choices,
            choiceNames = badgeType(
              col_name = var_choices,
              col_type = col_type(data[, var_choices, drop = TRUE])
            ),
            badge = FALSE
          )
        }
      }, ignoreNULL = FALSE)
      
      observeEvent(input$dragvars$target, {
        res_rv$aes <- input$dragvars$target
      })
      
      return(reactive(res_rv$aes))
    }
  )
}
