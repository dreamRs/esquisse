
### Whole Shiny app ###

library(shiny)
library(esquisse)


# Load some datasets in app environment
my_data <- data.frame(
  var1 = rnorm(100),
  var2 = sample(letters[1:5], 100, TRUE)
)


ui <- fluidPage(
  theme = bs_theme_esquisse(),
  esquisse_ui(
    id = "esquisse",
    header = list(close = FALSE), # hide the close button
    container = esquisseContainer(fixed = TRUE),
    play_pause = FALSE,
    controls = c("labs", "parameters", "appearance", "filters", "code", "export"),
    layout_sidebar = TRUE
  )
)

server <- function(input, output, session) {

  esquisse_server(id = "esquisse")

}

if (interactive())
  shinyApp(ui, server)


