

# Packages ----------------------------------------------------------------

library(shiny)
library(esquisse)




# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$h2("Choose data module"),
  fluidRow(
    column(
      width = 4,
      tags$h4("Default"),
      chooseDataUI(id = "choose1"),
      verbatimTextOutput(outputId = "res1")
    ),
    column(
      width = 4,
      tags$h4("No var selection"),
      chooseDataUI(id = "choose2"),
      verbatimTextOutput(outputId = "res2")
    ),
    column(
      width = 4,
      tags$h4("Default data on start"),
      chooseDataUI(id = "choose3"),
      verbatimTextOutput(outputId = "res3")
    )
  )
)



# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  res_dat1 <- callModule(
    chooseDataServer, id = "choose1",
    launchOnStart = FALSE
  )
  output$res1 <- renderPrint({
    str(reactiveValuesToList(res_dat1))
  })
  
  res_dat2 <- callModule(
    chooseDataServer, id = "choose2", selectVars = FALSE,
    launchOnStart = FALSE
  )
  output$res2 <- renderPrint({
    str(reactiveValuesToList(res_dat2))
  })
  
  res_dat3 <- callModule(
    chooseDataServer, id = "choose3", data = iris,
    launchOnStart = FALSE
  )
  output$res3 <- renderPrint({
    str(reactiveValuesToList(res_dat3))
  })
  
}



# App ---------------------------------------------------------------------

shinyApp(ui, server)
