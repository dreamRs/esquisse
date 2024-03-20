
library(shiny)
library(bslib)

ui <- fluidPage(
  tags$h2("Add / remove panels"),
  navset_pill(
    id = "navset_geoms",
    # title = "Geoms",
    # nav_item("Geoms"),
    nav_panel("Geom 1", "Geom un", select_aes_ui("geom1")),
    nav_panel("Geom 2", "Geom deux", select_aes_ui("geom2")),
    nav_spacer(),
    nav_item(actionButton("rm", ph("minus"))),
    nav_item(actionButton("add", ph("plus")))
  )
)

server <- function(input, output, session) {
  
  select_aes_server("geom1")
  select_aes_server("geom2")
  
  count_geom <- reactiveVal(2)
  
  observeEvent(input$add, {
    num <- count_geom() + 1
    nav_insert(
      id = "navset_geoms",
      nav = nav_panel(paste("Geom", num), paste("New geom", num)),
      target = paste("Geom", num - 1), 
      position = "after"
    )
    count_geom(num)
  })

  observeEvent(input$rm, {
    num <- count_geom()
    if (num > 1) {
      nav_remove(id = "navset_geoms", target = paste("Geom", num))
      count_geom(num - 1)
    }
  })
  
}

shinyApp(ui, server)