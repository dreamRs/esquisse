
library("shiny")
library(shinyWidgets)

ui <- fluidPage(
  imageButtonUI(
    id = "geom",
    imgs = list(
      list(inputId = "auto", img = "gg-auto.png", label = "Auto"),
      list(inputId = "line", img = "gg-line.png", label = "Line"),
      list(inputId = "bar", img = "gg-bar.png", label = "Bar"),
      list(inputId = "histo", img = "gg-histo.png", label = "Histo"),
      list(inputId = "point", img = "gg-point.png", label = "point"),
      list(inputId = "boxplot", img = "gg-boxplot.png", label = "Boxplot"),
      list(inputId = "density", img = "gg-density.png", label = "Density")
    ), width = "200px"
  ),
  verbatimTextOutput(outputId = "result"),
  actionButton(inputId = "test", label = "test")
)

server <- function(input, output, session) {

  r_enabled <- reactiveValues(x = c("auto", "line", "bar", "point", "density"))
  observeEvent(input$test, {
    r_enabled$x <- sample(c("auto", "line", "bar", "histo", "point", "boxplot", "density"), 4)
  })


  geom <- callModule(module = imageButtonServer, id = "geom", default = "auto", img_ref = list(
    auto = "gg-auto.png", line = "gg-line.png", bar = "gg-bar.png", histo = "gg-histo.png",
    point = "gg-point.png", boxplot = "gg-boxplot.png", density = "gg-density.png"
  ), enabled = r_enabled)

  output$result <- renderPrint({
    geom$x
  })
}

shinyApp(ui = ui, server = server)
