
if (interactive()) {
  
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
    esquisserUI(
      id = "esquisse", 
      header = FALSE, # dont display gadget title
      choose_data = FALSE, # dont display button to change data,
      container = esquisseContainer(height = "700px")
    )
  )
  
  server <- function(input, output, session) {
    
    data_r <- reactiveValues(data = iris, name = "iris")
    
    observeEvent(input$data, {
      if (input$data == "iris") {
        data_r$data <- iris
        data_r$name <- "iris"
      } else {
        data_r$data <- mtcars
        data_r$name <- "mtcars"
      }
    })
    
    callModule(module = esquisserServer, id = "esquisse", data = data_r)
    
  }
  
  shinyApp(ui, server)
  
  
  
  ### Whole Shiny app ###
  
  library(shiny)
  library(esquisse)
  
  
  # Load some datasets in app environment
  my_data <- data.frame(
    var1 = rnorm(100),
    var2 = sample(letters[1:5], 100, TRUE)
  )
  
  
  ui <- fluidPage(
    esquisserUI(
      id = "esquisse", 
      container = esquisseContainer(fixed = TRUE)
    )
  )
  
  server <- function(input, output, session) {
    
    callModule(module = esquisserServer, id = "esquisse")
    
  }
  
  shinyApp(ui, server)
  
  
  
  ## You can also use a vector of margins for the fixed argument,
  # useful if you have a navbar for example
  
  ui <- navbarPage(
    title = "My navbar app",
    tabPanel(
      title = "esquisse",
      esquisserUI(
        id = "esquisse", 
        header = FALSE,
        container = esquisseContainer(
          fixed = c(50, 0, 0, 0)
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    callModule(module = esquisserServer, id = "esquisse")
    
  }
  
  shinyApp(ui, server)
  
}
