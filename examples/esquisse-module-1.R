
### Part of a Shiny app ###

library(shiny)
library(esquisse)

ui <- fluidPage(
  tags$h1("Use esquisse as a Shiny module"),
  
  radioButtons(
    inputId = "data", 
    label = "Data to use:", 
    choices = c("iris", "mtcars"),
    inline = TRUE
  ),
  checkboxGroupInput(
    inputId = "aes", 
    label = "Aesthetics to use:", 
    choices = c(
      "fill", "color", "size", "shape", 
      "weight", "group", "facet", "facet_row", "facet_col"
    ),
    selected = c("fill", "color", "size", "facet"),
    inline = TRUE
  ),
  esquisse_ui(
    id = "esquisse", 
    header = FALSE, # dont display gadget title
    container = esquisseContainer(height = "700px")
  )
)

server <- function(input, output, session) {
  
  data_rv <- reactiveValues(data = iris, name = "iris")
  
  observeEvent(input$data, {
    if (input$data == "iris") {
      data_rv$data <- iris
      data_rv$name <- "iris"
    } else {
      data_rv$data <- mtcars
      data_rv$name <- "mtcars"
    }
  })
  
  esquisse_server(
    id = "esquisse", 
    data_rv = data_rv, 
    default_aes = reactive(input$aes)
  )
  
}

if (interactive())
  shinyApp(ui, server)

