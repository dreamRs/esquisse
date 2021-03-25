
# palettePicker -----------------------------------------------------------

library(shiny)
library(esquisse)
library(scales)

ui <- fluidPage(
  tags$h2("pickerColor examples"),
  
  fluidRow(
    column(
      width = 4,
      palettePicker(
        inputId = "pal1", 
        label = "Select a palette:", 
        choices = list(
          "Blues" = brewer_pal(palette = "Blues")(8),
          "Reds" = brewer_pal(palette = "Reds")(8)
        )
      ),
      verbatimTextOutput("res1"),
      palettePicker(
        inputId = "pal4", 
        label = "Update palette:", 
        choices = list(
          "Blues" = brewer_pal(palette = "Blues")(8),
          "Reds" = brewer_pal(palette = "Reds")(8)
        )
      ),
      verbatimTextOutput("res4"),
      radioButtons(
        "update", "Palettes:", c("default", "viridis", "brewer"),
        inline = TRUE
      )
    ),
    column(
      width = 4,
      palettePicker(
        inputId = "pal2", 
        label = "With a list of palette:", 
        choices = list(
          "Viridis" = list(
            "viridis" = viridis_pal(option = "viridis")(10),
            "magma" = viridis_pal(option = "magma")(10),
            "inferno" = viridis_pal(option = "inferno")(10),
            "plasma" = viridis_pal(option = "plasma")(10),
            "cividis" = viridis_pal(option = "cividis")(10)
          ),
          "Brewer" = list(
            "Blues" = brewer_pal(palette = "Blues")(8),
            "Reds" = brewer_pal(palette = "Reds")(8),
            "Paired" = brewer_pal(palette = "Paired")(8),
            "Set1" = brewer_pal(palette = "Set1")(8)
          )
        ), 
        textColor = c(
          rep("white", 5), rep("black", 4) 
        )
      ),
      verbatimTextOutput("res2")
    ),
    column(
      width = 4,
      palettePicker(
        inputId = "pal3", 
        label = "With plain colors:", 
        choices = list(
          "BrBG" = brewer_pal(palette = "BrBG")(8), 
          "PiYG" = brewer_pal(palette = "PiYG")(8), 
          "PRGn" = brewer_pal(palette = "PRGn")(8), 
          "PuOr" = brewer_pal(palette = "PuOr")(8), 
          "RdBu" = brewer_pal(palette = "RdBu")(8), 
          "RdGy" = brewer_pal(palette = "RdGy")(8), 
          "RdYlBu" = brewer_pal(palette = "RdYlBu")(8), 
          "RdYlGn" = brewer_pal(palette = "RdYlGn")(8), 
          "Spectral" = brewer_pal(palette = "Spectral")(8)
        ), 
        plainColor = TRUE, 
        textColor = "white"
      ),
      verbatimTextOutput("res3")
    )
  )
)

server <- function(input, output, session) {
  
  output$res1 <- renderPrint(input$pal1)
  output$res2 <- renderPrint(input$pal2)
  output$res3 <- renderPrint(input$pal3)
  output$res4 <- renderPrint(input$pal4)
  
  observeEvent(input$update, {
    if (input$update == "default") {
      updatePalettePicker(
        inputId = "pal4",
        choices = list(
          "Blues" = brewer_pal(palette = "Blues")(8),
          "Reds" = brewer_pal(palette = "Reds")(8)
        )
      )
    } else if (input$update == "viridis") {
      updatePalettePicker(
        inputId = "pal4",
        choices = list(
          "viridis" = viridis_pal(option = "viridis")(10),
          "magma" = viridis_pal(option = "magma")(10),
          "inferno" = viridis_pal(option = "inferno")(10),
          "plasma" = viridis_pal(option = "plasma")(10),
          "cividis" = viridis_pal(option = "cividis")(10)
        ),
        textColor = "#FFF"
      )
    } else if (input$update == "brewer") {
      updatePalettePicker(
        inputId = "pal4",
        choices = list(
          "Blues" = brewer_pal(palette = "Blues")(8),
          "Reds" = brewer_pal(palette = "Reds")(8),
          "Paired" = brewer_pal(palette = "Paired")(8),
          "Set1" = brewer_pal(palette = "Set1")(8)
        )
      )
    }
  })
}

if (interactive()) {
  shinyApp(ui, server)
}
