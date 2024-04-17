
# Show data -------------------------------------------------------------------------

#' @importFrom shiny NS
show_data_ui <- function(id) {
  ns <- NS(id)
  btn_header(i18n("Show data"), "table")(ns("btn"))
}

#' @importFrom shiny moduleServer observeEvent showNotification reactive
#' @importFrom datamods show_data
show_data_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$btn, {
        data <- data_r()
        if (!is.data.frame(data)) {
          showNotification(
            ui = "No data to display",
            duration = 700,
            id = paste("esquisse", sample.int(1e6, 1), sep = "-"),
            type = "warning"
          )
        } else {
          show_data(data, title = i18n("Dataset"), type = "modal")
        }
      })
      
    }
  )
}




# Update vars -----------------------------------------------------------------------

#' @importFrom shiny NS
update_vars_ui <- function(id) {
  ns <- NS(id)
  btn_header(i18n("Update variables"), "brackets-angle")(ns("btn"))
}

#' @importFrom shiny moduleServer observeEvent modalDialog showModal reactive
#' @importFrom datamods update_variables_ui update_variables_server
update_vars_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$btn, {
        showModal(modalDialog(
          title = tagList(
            i18n("Update & select variables"),
            button_close_modal()
          ),
          datamods::update_variables_ui(ns("update_variable"), title = NULL),
          easyClose = TRUE,
          size = "l",
          footer = NULL
        ))
      })
      updated_data <-datamods::update_variables_server(
        id = "update_variable",
        data = data_r
      )
      return(updated_data)
    }
  )
}
