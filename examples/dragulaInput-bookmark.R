library(shiny)
library(esquisse)

ui <- function(request) {
  fluidPage(
    tags$h2("Bookmark dragulaInput"),
    tags$br(),
    dragulaInput(
      inputId = "dad1",
      label = "Default:",
      sourceLabel = "Source",
      targetsLabels = c("Target 1", "Target 2"),
      choices = month.abb,
      width = "100%"
    ),
    verbatimTextOutput(outputId = "result1"),
    bookmarkButton(id = "bookmark1")
  )
}


server <- function(input, output, session) {
  
  output$result1 <- renderPrint(str(input$dad1))
  
  setBookmarkExclude(c("bookmark1"))
  # Trigger bookmarking
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  
}

enableBookmarking(store = "url")
shinyApp(ui, server)

