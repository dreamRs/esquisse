
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
    header = esquisse_header(
      close = FALSE, # hide the close button
      .after = actionButton( # custom button
        inputId = "open_modal",
        label = NULL,
        icon = icon("plus")
      )
    ),
    container = esquisse_container(fixed = TRUE),
    play_pause = FALSE,
    controls = c("settings", "labs", "axes", "geoms", "theme", "filters", "code", "export"),
    layout_sidebar = TRUE
  )
)

server <- function(input, output, session) {

  esquisse_server(id = "esquisse")


  observeEvent(input$open_modal, {
    showModal(modalDialog("Some content"))
  })
}

if (interactive())
  shinyApp(ui, server)


