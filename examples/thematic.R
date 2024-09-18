library(shiny)
library(ggplot2)
library(thematic)
# In order for auto/custom fonts to work properly, you'll want
# either the ragg (or showtext) package installed
library(ragg)

# If you want `{ragg}` to handle the font rendering in a Shiny app
options(shiny.useragg = TRUE)

# Call thematic_shiny() prior to launching the app, to change
# R plot theming defaults for all the plots generated in the app
thematic_shiny(font = "auto")

ui <- fluidPage(
  # bslib makes it easy to customize CSS styles for things
  # rendered by the browser, like tabsetPanel()
  # https://rstudio.github.io/bslib
  theme = bslib::bs_theme(
    bg = "#002B36", fg = "#EEE8D5", primary = "#2AA198",
    # bslib also makes it easy to import CSS fonts
    base_font = bslib::font_google("Pacifico")
  ),
  
  esquisse_ui(
    id = "esquisse",
    header = FALSE,
    container = esquisse_container(height = "700px")
  )
)

server <- function(input, output) {

  esquisse_server(
    id = "esquisse",
    data_rv = reactive(palmerpenguins::penguins)
  )
  
}

shinyApp(ui, server)
