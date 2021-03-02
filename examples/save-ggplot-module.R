library(shiny)
library(ggplot2)
library(shinyWidgets)


ui <- fluidPage(
  tags$h2("Save a ggplot"),
  selectInput("var", "Variable:", names(economics)[-1]),
  plotOutput("plot", width = "600px"),
  actionButton("save", "Save this plot")
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(plot = NULL)
  
  output$plot <- renderPlot({
    rv$plot <- ggplot(economics) + 
      geom_line(aes(date, !!sym(input$var))) + 
      theme_minimal()
    rv$plot
  })
  
  observeEvent(input$save, {
    save_ggplot_modal("ID", "Save plot")
  })
  save_ggplot_server("ID", rv)
}

if (interactive())
  shinyApp(ui, server)
