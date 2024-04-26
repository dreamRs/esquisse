# pkgload::load_all()

library(shiny)
library(ggplot2)
library(shinyWidgets)
library(rlang)

dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

if (FALSE) {


  ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    annotate(
      geom = "curve", x = 4, y = 35, xend = 2.65, yend = 27,
      curvature = .3, arrow = arrow(length = unit(2, "mm"))
    ) +
    annotate(geom = "text", x = 4.1, y = 35, label = "subaru", hjust = "left")


  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point()

  annot_list <- list(
    annotate(
      geom = "curve", x = 4, y = 35, xend = 2.65, yend = 27,
      curvature = .3, arrow = arrow(length = unit(2, "mm"))
    ),
    annotate(geom = "text", x = 4.1, y = 35, label = "subaru", hjust = "left")
  )

  annot_list2 <- list(
    annot1 = annotate(
      geom = "curve", x = 4, y = 35, xend = 2.65, yend = 27,
      curvature = .3, arrow = arrow(length = unit(2, "mm"))
    ),
    annot2 = annotate(geom = "text", x = 4.1, y = 35, label = "subaru", hjust = "left")
  )

  p + annot_list2

}



ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, preset = "bootstrap") |>
    bslib::bs_add_rules(c(
      ".btn-select-from-to { flex: 1; @extend .text-start; }",
      ".btn-check:checked+.btn.btn-select-from-to { background-color: #FFF; color: #000;}",
      ".btn-check:checked+.btn.btn-select-from-to { @extend .border }",
      ".btn-check:checked+.btn.btn-select-from-to { @extend .border-primary }",
      ".btn-check:checked+.btn.btn-select-from-to { @extend .border-2 }"
    )),
  html_dependency_winbox(),
  tags$h2("Annotation dev"),
  fluidRow(
    column(
      width = 6,
      plotOutput(outputId = "plot", click = "plot_click"),
      actionButton("add", "Add annotation")
    ),
    column(
      width = 6,
      verbatimTextOutput("res")
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(annotations = list())

  output$plot <- renderPlot({
    p <- ggplot(mpg) +
      aes(displ, cty) +
      geom_point()
    if (length(rv$annotations) > 0) {
      print(str(rv$annotations))
      p <- p + rv$annotations
    }
    p
  })

  output$res <- renderPrint({
    input$plot_click
  })

  observeEvent(input$plot_click, {
    updateNumericInput(inputId = paste0("x_", input$from_to), value = input$plot_click$x)
    updateNumericInput(inputId = paste0("y_", input$from_to), value = input$plot_click$y)
  })
  observeEvent(input$add, {
    WinBox(
      title = "Add an annotation",
      ui = tagList(
        radioGroupButtons(
          inputId = "from_to",
          label = NULL,
          choiceNames = tagList(
            tags$div(
              tags$b("From:"),
              splitLayout(
                numericInputIcon("x_from", NULL, icon = list("x:"), value = NA, size = "sm", width = "100%"),
                numericInputIcon("y_from", NULL, icon = list("y:"), value = NA, size = "sm", width = "100%")
              )
            ),
            tags$div(
              tags$b("To:"),
              splitLayout(
                numericInputIcon("x_to", NULL, icon = list("x:"), value = NA, size = "sm", width = "100%"),
                numericInputIcon("y_to", NULL, icon = list("xy:"), value = NA, size = "sm", width = "100%")
              )
            )
          ),
          choiceValues = c("from", "to"),
          individual = TRUE,
          # justified = TRUE,
          status = "outline-secondary btn-select-from-to rounded-1 my-1",
          width = "100%"
        ),
        prettyRadioButtons(
          inputId = "type_geom",
          label = "Type:",
          choices = c("segment", "curve"),
          status = "primary",
          inline = TRUE,
          outline = TRUE
        ),
        sliderInput(
          inputId = "curvature",
          "Curvature:",
          min = 0,
          max = 1,
          value = 0.3,
          step = 0.1,
          width = "100%"
        ),
        prettyCheckbox(
          inputId = "arrow",
          label = "Add an arrow?",
          value = FALSE,
          status = "primary",
          outline = TRUE
        ),
        sliderInput(
          inputId = "arrow_length",
          "Arrow length:",
          min = 1,
          max = 5,
          value = 2,
          step = 0.5,
          width = "100%"
        )#,
        # actionButton(
        #   inputId = "add_annotation",
        #   label = "Add annotation",
        #   class = "btn-outline-primary",
        #   width = "100%"
        # )
      ),
      options = wbOptions(width = "300px"),
      controls = wbControls(min = FALSE, max = FALSE, full = FALSE)
    )
  })

  bindEvent(
    observe({
      if (isTruthy(input$x_from) & isTruthy(input$y_from) & isTruthy(input$x_to) & isTruthy(input$y_to)) {
        args_annot <- dropNulls(list(
          geom = input$type_geom,
          x = input$x_from,
          y = input$y_from,
          xend = input$x_to,
          yend = input$y_to,
          curvature = if (identical(input$type_geom, "curve")) input$curvature,
          arrow = if (isTRUE(input$arrow)) arrow(length = unit(input$arrow_length, "mm"))
        ))
        rv$annotations[[paste0("annotatio_", input$add)]] <- exec("annotate", !!!args_annot)
      } else {
        rv$annotations[[paste0("annotatio_", input$add)]] <- NULL
      }
    }),
    input$x_from, input$y_from, input$x_to, input$y_to,
    input$type_geom, input$curvature, input$arrow,
    input$arrow_length
  )

}

shinyApp(ui, server)
