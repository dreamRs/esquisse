
## You can also use a vector of margins for the fixed argument,
# useful if you have a navbar for example

library(shiny)
library(esquisse)
library(datamods)

ui <- navbarPage(
  title = "My navbar app",
  tabPanel(
    title = "esquisse",
    esquisse_ui(
      id = "esquisse", 
      header = FALSE,
      container = esquisseContainer(
        fixed = c(55, 0, 0, 0)
      )
    )
  )
)

server <- function(input, output, session) {
  
  # lauch import data modal
  import_modal(
    id = "import-data",
    from = c("env", "file", "copypaste"),
    title = "Import data"
  )
  data_imported_r <- datamods::import_server("import-data")

  data_rv <- reactiveValues(data = data.frame())
  observeEvent(data_imported_r$data(), {
    data_rv$data <- data_imported_r$data()
    data_rv$name <- data_imported_r$name()
  })
  
  esquisse_server(id = "esquisse", data_rv = data_rv)
  
}

if (interactive())
  shinyApp(ui, server)
