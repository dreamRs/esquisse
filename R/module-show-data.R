
#' @importFrom shiny NS actionButton
#' @importFrom htmltools tagList
#' @importFrom phosphoricons ph
show_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("show_data"),
      label = ph("table", height = "2em", title = i18n("Show data")),
      class = "btn-sm",
      title = i18n("Show data")
    )
  )
}

#' @importFrom shiny moduleServer observeEvent showNotification reactive
#' @importFrom datamods show_data
show_data_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show_data, {
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
