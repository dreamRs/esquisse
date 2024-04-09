

library(shiny)
library(bslib)
pkgload::load_all()

ui <- fluidPage(
  theme = bs_theme_esquisse(),
  html_dependency_esquisse(),
  tags$h2("Select geom & aes"),
  # select_geom_aes_ui("myid", n_geoms = 1),
  select_geom_aes_ui(
    "myid",
    n_geoms = 5,
    list_geoms = list(
      geomIcons(),
      geomIcons(c("line", "step", "jitter", "point", "smooth", "density", "boxplot", "violin"), default = "blank"),
      geomIcons(c("line", "step", "jitter", "point", "smooth", "density", "boxplot", "violin"), default = "blank"),
      geomIcons(c("line", "step", "jitter", "point", "smooth", "density", "boxplot", "violin"), default = "blank")
    )
  ),
  verbatimTextOutput("result")
)

server <- function(input, output, session) {

  res_r <- select_geom_aes_server(
    id = "myid",
    n_geoms = 4,
    data_r = reactive(palmerpenguins::penguins)
    # data_r = reactive(apexcharter::temperatures)
  )
  output$result <- renderPrint(str(res_r()))

}

shinyApp(ui, server)
