if (interactive()) {
  
  library(shiny)
  library(shinyWidgets)
  library(ggplot2)
  library(esquisse)
  
  # Add some NAs to mpg
  mpg_na <- mpg
  mpg_na[] <- lapply(
    X = mpg_na,
    FUN = function(x) {
      x[sample.int(n = length(x), size = sample(15:30, 1))] <- NA
      x
    }
  )
  
  ui <- fluidPage(
    tags$h2("Filter data.frame"),
    
    radioButtons(
      inputId = "dataset", 
      label = "Data:",
      choices = c(
        "iris", "mtcars", "economics", 
        "midwest", "mpg", "mpg_na", "msleep", "diamonds",
        "faithfuld", "txhousing"
      ),
      inline = TRUE
    ),
    
    fluidRow(
      column(
        width = 3,
        filterDF_UI("filtering")
      ),
      column(
        width = 9,
        progressBar(
          id = "pbar", value = 100, 
          total = 100, display_pct = TRUE
        ),
        DT::dataTableOutput(outputId = "table"),
        tags$p("Code dplyr:"),
        verbatimTextOutput(outputId = "code_dplyr"),
        tags$p("Expression:"),
        verbatimTextOutput(outputId = "code"),
        tags$p("Filtered data:"),
        verbatimTextOutput(outputId = "res_str")
      )
    )
  )
  
  server <- function(input, output, session) {
    
    data <- reactive({
      get(input$dataset)
    })
    
    res_filter <- callModule(
      module = filterDF, 
      id = "filtering", 
      data_table = data,
      data_name = reactive(input$dataset)
    )
    
    observeEvent(res_filter$data_filtered(), {
      updateProgressBar(
        session = session, id = "pbar", 
        value = nrow(res_filter$data_filtered()), total = nrow(data())
      )
    })
    
    output$table <- DT::renderDT({
      res_filter$data_filtered()
    }, options = list(pageLength = 5))
    
    
    output$code_dplyr <- renderPrint({
      res_filter$code$dplyr
    })
    output$code <- renderPrint({
      res_filter$code$expr
    })
    
    output$res_str <- renderPrint({
      str(res_filter$data_filtered())
    })
    
  }
  
  shinyApp(ui, server)
  
}
