
#' @title Save \code{ggplot} module
#' 
#' @description Save a \code{ggplot} object in various format and resize it before saving.
#'
#' @param id Module's ID.
#'
#' @return No value. Use in UI & server of shiny application.
#' @export
#' 
#' @name save-ggplot-module
#' 
#' @importFrom shiny NS plotOutput actionButton downloadButton textInput icon
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets textInputIcon numericInputIcon
#'
#' @example examples/save-ggplot-module.R
save_ggplot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    html_dependency_moveable(),
    tags$div(plotOutput(ns("plot"))),
    tags$br(),
    tags$div(
      style = "display: grid;",
      style = "grid-template-columns: 4fr 2fr 2fr 2fr;",
      style = "grid-column-gap: 10px;",
      style = "width: 100%;", # 868px
      textInputIcon(
        inputId = ns("filename"),
        label = NULL,
        value = "export-plot",
        placeholder = "Filename",
        icon = list("Filename:"),
        width = "100%"
      ),
      numericInputIcon(
        inputId = ns("width"),
        label = NULL,
        value = 868,
        icon = list("Width:"),
        width = "100%"
      ),
      numericInputIcon(
        inputId = ns("height"),
        label = NULL,
        value = 400,
        icon = list("Height:"),
        width = "100%"
      ),
      actionButton(
        inputId = ns("update_preview"),
        label = "Update Preview",
        icon = icon("eye"),
        style = "margin-bottom: 15px;"
      )
    ),
    tags$div(
      tags$b("Export format:"),
      tags$br(),
      downloadButton(outputId = ns("png"), label = "PNG", style = "width: 120px;"),
      downloadButton(outputId = ns("pdf"), label = "PDF", style = "width: 120px;"),
      downloadButton(outputId = ns("svg"), label = "SVG", style = "width: 120px;"),
      downloadButton(outputId = ns("jpeg"), label = "JPEG", style = "width: 120px;"),
      downloadButton(outputId = ns("bmp"), label = "BMP", style = "width: 120px;"),
      downloadButton(outputId = ns("eps"), label = "EPS", style = "width: 120px;"),
      downloadButton(outputId = ns("tiff"), label = "TIFF", style = "width: 120px;")
    ),
    tags$div(
      style = "display: none;",
      textInput(inputId = ns("hidden"), label = NULL, value = genId())
    )
  )
}

#' @param title Modal's title.
#' 
#' @export
#' 
#' @rdname save-ggplot-module
#' 
#' @importFrom shiny NS showModal modalDialog checkboxInput
#' @importFrom htmltools tagList tags
save_ggplot_modal <- function(id, title = NULL) {
  ns <- NS(id)
  showModal(modalDialog(
    title = tagList(
      tags$button(
        icon("close"),
        class = "btn btn-default pull-right",
        style = "border: 0 none;",
        `data-dismiss` = "modal",
        `aria-label` = "Close"
      ),
      title
    ),
    footer = NULL,
    size = "l",
    fade = FALSE,
    save_ggplot_ui(id),
    tags$div(
      style = "display: none;",
      checkboxInput(inputId = ns("modal"), label = NULL, value = TRUE)
    )
  ))
}

#' @param plot_rv A \code{reactiveValues} with a slot \code{plot} containing a \code{ggplot} object.
#' 
#' @export
#' 
#' @rdname save-ggplot-module
#' 
#' @importFrom shiny moduleServer observeEvent req renderPlot isTruthy
#' @importFrom shinyWidgets updateNumericInputIcon
save_ggplot_server <- function(id, plot_rv) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      ns <- session$ns
      plot_width <- paste0("output_", ns("plot"), "_width")
      plot_height <- paste0("output_", ns("plot"), "_height")
      
      observeEvent(input$hidden, {
        activate_resizer(id = ns("plot"), modal = isTRUE(input$modal))
      })
      
      observeEvent(input$update_preview, {
        if (isTruthy(input$width) & isTruthy(input$height)) {
          resize(
            id = ns("plot"),
            width = input$width,
            height = input$height
          )
        }
      })
      observeEvent(session$clientData[[plot_width]], {
        updateNumericInputIcon(
          session = session,
          inputId = "width",
          value = session$clientData[[plot_width]]
        )
      })
      observeEvent(session$clientData[[plot_height]], {
        updateNumericInputIcon(
          session = session,
          inputId = "height",
          value = session$clientData[[plot_height]]
        )
      })
      
      output$plot <- renderPlot({
        req(plot_rv$plot)
        plot_rv$plot
      })
      
      output$png <- download_plot_rv(input, plot_rv, "png")
      output$pdf <- download_plot_rv(input, plot_rv, "pdf")
      output$bmp <- download_plot_rv(input, plot_rv, "bmp")
      output$svg <- download_plot_rv(input, plot_rv, "svg")
      output$tiff <- download_plot_rv(input, plot_rv, "tiff")
      output$eps <- download_plot_rv(input, plot_rv, "eps")
      output$jpeg <- download_plot_rv(input, plot_rv, "jpeg")
      
      return(NULL)
    }
  )
}

#' @importFrom shiny downloadHandler
#' @importFrom ggplot2 ggsave
download_plot_rv <- function(input, rv, device) {
  downloadHandler(
    filename = function() {
      filename <- input$filename
      if (endsWith(filename, paste0("\\.", device)))
        filename
      else
        paste0(filename, ".", device)
    },
    content = function(file) {
      width <- input$width
      height <- input$height
      ggsave(
        filename = file,
        plot = rv$plot,
        device = device,
        dpi = 72,
        width = width / 72,
        height = height / 72,
        scale = 1
      )
    }
  )
}
