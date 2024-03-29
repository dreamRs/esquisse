
library(shiny)
library(ggplot2)
library(esquisse)
library(plotly)


ui <- fluidPage(
  tags$h2("ggplot/plotly output"),
  radioButtons(
    inputId = "type",
    label = "Render with:",
    choices = c("plot", "plotly"),
    inline = TRUE
  ),
  selectInput("var", "Variable:", names(economics)[-1]),
  ggplot_output("MYID", width = "600px")
)

server <- function(input, output, session) {
  
  render_ggplot("MYID", {
    ggplot(economics) + 
      geom_line(aes(date, !!sym(input$var))) + 
      theme_minimal() + 
      labs(
        title = "A cool chart made with ggplot2",
        subtitle = "that you can export in various format"
      )
  }, use_plotly = reactive(identical(input$type, "plotly")))
}

if (interactive())
  shinyApp(ui, server)
