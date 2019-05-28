

# Packages ----------------------------------------------------------------

library(shiny)
library(esquisse)




# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$h2("Import an external file"),
  chooseDataUI(id = "import", label = "Import a file"),
  verbatimTextOutput(outputId = "result")
)



# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  res_import <- callModule(
    module = chooseDataServer, 
    id = "import",
    dataModule = "ImportFile",
    launchOnStart = FALSE
  )
  output$result <- renderPrint({
    str(reactiveValuesToList(res_import))
  })
  
}



# App ---------------------------------------------------------------------

shinyApp(ui, server)
