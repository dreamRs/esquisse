
# colorPicker -------------------------------------------------------------

if (interactive()) {
  
  library(shiny)
  library(esquisse)
  library(scales)
  
  
  ui <- fluidPage(
    tags$h2("colorPicker examples"),
    fluidRow(
      column(
        width = 3,
        colorPicker(
          inputId = "col1",
          label = "With a vector of colors",
          choices = brewer_pal(palette = "Dark2")(8)
        ),
        verbatimTextOutput("res1")
      ),
      column(
        width = 3,
        colorPicker(
          inputId = "col2",
          label = "Change text color",
          choices = brewer_pal(palette = "Blues")(8), 
          textColor = c("black", "black", "black", "white",
                        "white", "white", "white", "white")
        ),
        verbatimTextOutput("res2")
      ),
      column(
        width = 3,
        colorPicker(
          inputId = "col3",
          label = "With a list of vector of colors",
          choices = list(
            "Blues" = brewer_pal(palette = "Blues")(8),
            "Reds" = brewer_pal(palette = "Reds")(8),
            "Greens" = brewer_pal(palette = "Greens")(8)
          )
        ),
        verbatimTextOutput("res3")
      ),
      column(
        width = 3,
        colorPicker(
          inputId = "col4",
          label = "Plain color",
          choices = brewer_pal(palette = "Paired")(8), 
          plainColor = TRUE, 
          multiple = TRUE, 
          pickerOpts = list(`selected-text-format`= "count > 3")
        ),
        verbatimTextOutput("res4")
      )
    )
  )
  
  server <- function(input, output, session) {
    
    output$res1 <- renderPrint(input$col1)
    output$res2 <- renderPrint(input$col2)
    output$res3 <- renderPrint(input$col3)
    output$res4 <- renderPrint(input$col4)
    
  }
  
  shinyApp(ui, server)
  
}
