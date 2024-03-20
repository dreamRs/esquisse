

library(shiny)
library(bslib)
pkgload::load_all()

ui <- fluidPage(
  theme = bs_theme_esquisse(),
  html_dependency_esquisse(),
  tags$h2("Select geom & aes"),
  select_geom_aes_ui("myid"),
  verbatimTextOutput("result")
)

server <- function(input, output, session) {
  
  res_r <- select_geom_aes_server(
    id = "myid", 
    data_r = reactive(palmerpenguins::penguins)
  )
  output$result <- renderPrint(str(res_r()))
  
}

shinyApp(ui, server)
