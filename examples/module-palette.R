
pkgload::load_all()

library(shiny)

ui <- fluidPage(
  theme = bs_theme_esquisse(),
  fluidRow(
    column(
      width = 4,
      palette_ui("ID")
    ),
    column(
      width = 8,
      verbatimTextOutput("res")
    )
  )
)

server <- function(input, output, session) {
  
  res <- palette_server(
    "ID", 
    variable = reactive(
      palmerpenguins::penguins$species
    )
  )
  
  output$res <- renderPrint(res())
  
}

shinyApp(ui, server)
