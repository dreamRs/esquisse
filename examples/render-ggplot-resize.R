
library(shiny)
library(ggplot2)
library(esquisse)


ui <- fluidPage(
  tags$h2("ggplot output"),
  fluidRow(
    column(
      width = 3,
      selectInput("var", "Variable:", names(economics)[-1]),
      sliderInput("width", "Width:", 0, 1600, 800),
      sliderInput("height", "Height:", 0, 1600, 400),
      tags$p(
        "Width: ", textOutput("plot_width"),
        tags$br(),
        "Height: ", textOutput("plot_height")
      )
    ),
    column(
      width = 9,
      ggplot_output("MYID", width = "600px", height = "400px")
    )
  )
)

server <- function(input, output, session) {
  
  rv <- render_ggplot(
    id = "MYID",
    {
      ggplot(economics) + 
        geom_line(aes(date, !!sym(input$var))) + 
        theme_minimal() + 
        labs(
          title = "A cool chart made with ggplot2",
          subtitle = "that you can export in various format"
        )
    },
    resizable = TRUE, 
    width = reactive(input$width), 
    height = reactive(input$height)
  )
  
  output$plot_width <- renderText(rv$plot_width)
  output$plot_height <- renderText(rv$plot_height)
  
}

if (interactive())
  shinyApp(ui, server)
