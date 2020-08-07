library(shiny)
library(esquisse)

ui <- fluidPage(
  tags$h2("Demo dragulaInput"),
  tags$br(),
  fluidRow(
    column(
      width = 6,
      
      dragulaInput(
        inputId = "dad1",
        label = "Default:",
        sourceLabel = "Source",
        targetsLabels = c("Target 1", "Target 2"),
        choices = month.abb,
        width = "100%"
      ),
      verbatimTextOutput(outputId = "result1"),
      
      tags$br(),
      
      dragulaInput(
        inputId = "dad3",
        label = "On same row:",
        sourceLabel = "Source",
        targetsLabels = c("Target 1", "Target 2"),
        choices = month.abb,
        width = "100%",
        ncolSource = 1,
        ncolGrid = 3
      ),
      verbatimTextOutput(outputId = "result3")
    ),
    
    column(
      width = 6,
      dragulaInput(
        inputId = "dad2",
        label = "Two rows:",
        sourceLabel = "Source",
        targetsLabels = c("x", "y", "color", "fill", "size", "facet"),
        choices = names(mtcars),
        width = "100%",
        ncolGrid = 3
      ),
      verbatimTextOutput(outputId = "result2"),
      
      tags$br(),
      
      dragulaInput(
        inputId = "dad4",
        label = "Two rows not filled:",
        sourceLabel = "Source",
        targetsLabels = c("x", "y", "color", "fill", "size"),
        choices = names(mtcars),
        width = "100%",
        ncolGrid = 3
      ),
      verbatimTextOutput(outputId = "result4")
    )
  )
)


server <- function(input, output, session) {
  
  output$result1 <- renderPrint(str(input$dad1))
  
  output$result2 <- renderPrint(str(input$dad2))
  
  output$result3 <- renderPrint(str(input$dad3))
  
  output$result4 <- renderPrint(str(input$dad4))
  
}

if (interactive())
  shinyApp(ui = ui, server = server)


