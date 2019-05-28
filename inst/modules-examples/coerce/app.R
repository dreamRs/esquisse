

# Packages ----------------------------------------------------------------

library(esquisse)
library(shiny)


# Data --------------------------------------------------------------------

data("mpg", package = "ggplot2")




# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$h2("Coerce module"),
  coerceUI(id = "example"),
  verbatimTextOutput(outputId = "print_result"),
  verbatimTextOutput(outputId = "print_names")
)



# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  result <- callModule(
    module = coerceServer,
    id = "example", 
    data = reactive(mpg)
  )
  
  output$print_result <- renderPrint({
    str(result$data)
  })
  output$print_names <- renderPrint({
    result$names
  })
}



# App ---------------------------------------------------------------------

shinyApp(ui, server)
