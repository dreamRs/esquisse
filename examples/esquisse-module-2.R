
### Whole Shiny app ###

library(shiny)
library(esquisse)


# Load some datasets in app environment
my_data <- data.frame(
  var1 = rnorm(100),
  var2 = sample(letters[1:5], 100, TRUE)
)


ui <- fluidPage(
  esquisse_ui(
    id = "esquisse", 
    container = esquisseContainer(fixed = TRUE)
  )
)

server <- function(input, output, session) {
  
  esquisse_server(id = "esquisse")
  
}

if (interactive())
  shinyApp(ui, server)


