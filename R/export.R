

# Save ggplot -------------------------------------------------------------


#' @title Save `ggplot` module
#' 
#' @description Save a \code{ggplot} object in various format and resize it before saving.
#'
#' @param id Module ID.
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

#' @param plot_rv A `reactiveValues` with a slot `plot` containing a `ggplot` object.
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





# Render ggplot -----------------------------------------------------------


#' @title Render \code{ggplot} module
#' 
#' @description Display a plot on the client and allow to download it.
#'
#' @param id Module ID.
#' @param width Width of the plot.
#' @param height Height of the plot.
#' @param downloads Labels for export options, use `downloads_labels`.
#' @param ... Parameters passed to \code{\link[shiny:plotOutput]{plotOutput}} or \code{\link[shiny:renderPlot]{renderPlot}}.
#'
#' @return Server-side, a `reactiveValues` with the plot.
#' @export
#' 
#' @name ggplot-output
#' 
#' @importFrom shiny NS downloadLink actionButton plotOutput actionLink
#' @importFrom htmltools tags tagList
#' @importFrom shinyWidgets dropMenu
#'
#' @example examples/render-ggplot.R
ggplot_output <- function(id, width = "100%", height = "400px", downloads = downloads_labels(), ...) {
  ns <- NS(id)
  tags$div(
    class = "ggplot-container",
    style = "position: relative;",
    style = if (!is.null(width)) paste0("width:", validateCssUnit(width), ";"),
    style = if (!is.null(height)) paste0("height:", validateCssUnit(height), ";"),
    if (!is.null(downloads)) {
      e <- downloads[-1]
      e <- e[-length(e)]
      dlBtn <- lapply(
        X = seq_along(e),
        FUN = function(i) {
          if (is.null(e[[i]]))
            return(NULL)
          tagList(
            downloadLink(
              outputId = ns(paste0("export_", names(e)[i])),
              label = e[[i]]
            ),
            tags$br()
          )
        }
      )
      dropMenu(
        actionButton(
          inputId = ns("exports"),
          label = downloads$label,
          class = "btn-sm",
          style= "position: absolute; top: 0; right: 5px;"
        ),
        placement = "bottom-end",
        dlBtn,
        if (!is.null(downloads$more)) tagList(
          tags$hr(style = "margin: 5px 0;"),
          actionLink(inputId = ns("more"), label = downloads$more)
        )
      )
    },
    plotOutput(outputId = ns("plot"), width = width, height = height, ...)
  )
}

#' @param label Main label for export button
#' @param png,pdf,svg,jpeg,pptx Labels to display in
#'  export menu, use \code{NULL} to disable specific format.
#' @param more Label for "more" button, allowing to launch export modal.
#' 
#' @rdname ggplot-output
#' @export
downloads_labels <- function(label = icon("download"),
                             png = tagList(icon("file-image-o"), "PNG"),
                             pdf = tagList(icon("file-pdf-o"), "PDF"),
                             svg = tagList(icon("chrome"), "SVG"),
                             jpeg = tagList(icon("file-image-o"), "JPEG"),
                             pptx = tagList(icon("file-powerpoint-o"), "PPTX"),
                             more = tagList(icon("gear"), "More options")) {
  list(
    label = label,
    png = png,
    pdf = pdf,
    svg = svg,
    jpeg = jpeg,
    pptx = pptx,
    more = more
  )
}

#' @param expr An expression that generates a `ggplot` object.
#' @param env The environment in which to evaluate expression.
#' @param quoted Is `expr` a quoted expression (with `quote()`)? This
#'   is useful if you want to save an expression in a variable.
#' @param filename A string of the filename to export WITHOUT extension,
#'  it will be added according to type of export.
#' 
#' @rdname ggplot-output
#' 
#' @export
#' 
#' @importFrom shiny exprToFunction moduleServer downloadHandler
#'  reactiveValues renderPlot observeEvent showNotification is.reactive
#' @importFrom shinyWidgets hideDropMenu
render_ggplot <- function(id,
                          expr,
                          ...,
                          env = parent.frame(),
                          quoted = FALSE,
                          filename = "export-ggplot") {
  gg_fun <- exprToFunction(expr, env, quoted)
  moduleServer(
    id = id,
    module = function(input, output, session) {
      output$export_png <- download_plot_fun(gg_fun, "png", filename, session)
      output$export_pdf <- download_plot_fun(gg_fun, "pdf", filename, session)
      output$export_svg <- download_plot_fun(gg_fun, "svg", filename, session)
      output$export_jpeg <- download_plot_fun(gg_fun, "jpeg", filename, session)
      output$export_pptx <- downloadHandler(
        filename = function() {
          if (is.reactive(filename))
            filename <- filename()
          if (endsWith(filename, "\\.pptx"))
            filename
          else
            paste0(filename, ".pptx")
        },
        content = function(file) {
          if (requireNamespace(package = "rvg") & requireNamespace(package = "officer")) {
            gg <- gg_fun()
            ppt <- officer::read_pptx()
            ppt <- officer::add_slide(x = ppt, layout = "Blank")
            ppt <- try(officer::ph_with(
              x = ppt, rvg::dml(ggobj = gg), 
              location = officer::ph_location_fullsize()
            ), silent = TRUE)
            if ("try-error" %in% class(ppt)) {
              shiny::showNotification(
                ui = "Export to PowerPoint failed...", 
                type = "error", 
                id = paste("esquisse", sample.int(1e6, 1), sep = "-")
              )
            } else {
              tmp <- tempfile(pattern = "esquisse", fileext = ".pptx")
              print(ppt, target = tmp)
              file.copy(from = tmp, to = file)
            }
          } else {
            warn <- "Packages 'officer' and 'rvg' are required to use this functionality."
            warning(warn, call. = FALSE)
            shiny::showNotification(
              ui = warn, 
              type = "warning", 
              id = paste("esquisse", sample.int(1e6, 1), sep = "-")
            )
          }
        }
      )
      rv <- reactiveValues(plot = NULL)
      output$plot <- renderPlot({
        rv$plot <- gg_fun()
        rv$plot
      }, ...)
      observeEvent(input$more, {
        hideDropMenu("exports_dropmenu")
        save_ggplot_modal(
          id = session$ns("export"),
          title = "Export chart"
        )
      })
      save_ggplot_server("export", plot_rv = rv)
      return(rv)
    }
  )
}





# Utils donwload handlers -------------------------------------------------


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

#' @importFrom shiny downloadHandler
#' @importFrom ggplot2 ggsave
download_plot_fun <- function(fun, device, filename, session) {
  downloadHandler(
    filename = function() {
      if (is.reactive(filename))
        filename <- filename()
      if (endsWith(filename, paste0("\\.", device)))
        filename
      else
        paste0(filename, ".", device)
    },
    content = function(file) {
      name <- session$ns("plot")
      width <- paste0("output_", name, "_width")
      width <- session$clientData[[width]]
      height <- paste0("output_", name, "_height")
      height <- session$clientData[[height]]
      ggsave(
        filename = file,
        plot = fun(),
        device = device,
        dpi = 72,
        width = width / 72,
        height = height / 72,
        scale = 1
      )
    }
  )
}

