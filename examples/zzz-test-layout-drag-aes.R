

library(shiny)
pkgload::load_all()

ui <- fluidPage(
  
  tags$style(
    HTML("#dad4-target-6661636574 {grid-area: 1 / 6 / 3 / 7; height: auto !important;}"),
    HTML("#dad4-source-container { grid-area: 1 / 1 / 2 / 6; }")
  ),
  dragulaInput(
    inputId = "dad4",
    label = NULL,
    sourceLabel = "Source",
    targetsLabels = c("x", "y", "color", "fill", "size", "facet"),
    targetsIds = c("x", "y", "color", "fill", "size", "facet"),
    choices = names(mtcars),
    width = "100%",
    ncolGrid = 6,
    nrowGrid = 2,
    ncolSource = NULL,
    targetsHeight = "50px",
    replace = TRUE
  ),
  verbatimTextOutput(outputId = "result4"),
  tags$style(
    HTML("#dad5-target-6661636574 {grid-area: 1 / 6 / 4 / 7; height: auto !important;}"),
    HTML("#dad5-source-container { grid-area: 1 / 1 / 2 / 6; }")
  ),
  dragulaInput(
    inputId = "dad5",
    label = NULL,
    sourceLabel = "Source",
    targetsLabels = c(
      "x", "y", "color", "fill", "size",
      "x", "y", "color", "fill", "size",
      "facet"
    ),
    targetsIds = c(
      "x1", "y1", "color1", "fill1", "size1", 
      "x2", "y2", "color2", "fill2", "size2", 
      "facet"
    ),
    choices = names(mtcars),
    width = "100%",
    ncolGrid = 6,
    nrowGrid = 3,
    ncolSource = NULL,
    targetsHeight = "50px",
    replace = TRUE
  ),
  verbatimTextOutput(outputId = "result5")
)

server <- function(input, output, session) {
  output$result4 <- renderPrint(str(input$dad4))
  output$result5 <- renderPrint(str(input$dad5))
}

shinyApp(ui, server)
