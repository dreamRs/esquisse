
# Save ggplot -------------------------------------------------------------


#' @title Save `ggplot` module
#'
#' @description Save a `ggplot` object in various format and resize it before saving.
#'
#' @param id Module ID.
#' @param output_format Output formats offered to the user.
#'
#' @return No value. Use in UI & server of shiny application.
#' @export
#'
#' @name save-ggplot-module
#'
#' @importFrom shiny NS plotOutput actionButton downloadButton textInput
#' @importFrom htmltools tagList tags css
#' @importFrom shinyWidgets textInputIcon numericInputIcon
#'
#' @example examples/save-ggplot-module.R
save_ggplot_ui <- function(id, output_format = c("png", "pdf", "svg", "jpeg", "bmp", "eps", "tiff")) {
  ns <- NS(id)
  output_format <- match.arg(output_format, several.ok = TRUE)
  tagList(
    html_dependency_moveable(),
    tags$div(plotOutput(ns("plot"))),
    tags$br(),
    tags$div(
      style = css(
        display = "grid",
        gridTemplateColumns = "4fr 2fr 2fr 2fr",
        gridColumnGap = "10px",
        width = "100%"
      ),
      textInputIcon(
        inputId = ns("filename"),
        label = NULL,
        value = "export-plot",
        placeholder = i18n("Filename"),
        icon = list(i18n("Filename:")),
        width = "100%"
      ),
      numericInputIcon(
        inputId = ns("width"),
        label = NULL,
        value = 868,
        icon = list(i18n("Width:")),
        width = "100%"
      ),
      numericInputIcon(
        inputId = ns("height"),
        label = NULL,
        value = 400,
        icon = list(i18n("Height:")),
        width = "100%"
      ),
      actionButton(
        inputId = ns("update_preview"),
        label = tagList(ph("eye"), i18n("Update Preview")),
        style = "margin-bottom: 15px;",
        class = "btn-outline-primary text-nowrap"
      )
    ),
    tags$div(
      tags$label(i18n("Export format:")),
      tags$div(
        style = css(
          display = "grid",
          gridTemplateColumns = sprintf("repeat(%s, 1fr)", length(output_format)),
          gridColumnGap = "10px"
        ),
        lapply(
          X = output_format,
          FUN = function(x) {
            downloadButton(
              outputId = ns(x),
              label = tagList(ph("download"), toupper(x)),
              style = "width: 100%;",
              icon = NULL,
              class = "btn-sm btn-outline-primary"
            )
          }
        )
      )
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
save_ggplot_modal <- function(id,
                              title = NULL,
                              output_format = c("png", "pdf", "svg", "jpeg", "bmp", "eps", "tiff")) {
  ns <- NS(id)
  showModal(modalDialog(
    title = tagList(
      title,
      button_close_modal()
    ),
    footer = NULL,
    size = "xl",
    fade = FALSE,
    save_ggplot_ui(id, output_format = output_format),
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
#' @param width,height Width / Height of the plot, in the server it has to be a [shiny::reactive()] function returning a new width/height for the plot.
#' @param downloads Labels for export options, use `downloads_labels()` or `NULL` to disable export options.
#' @param ... Parameters passed to [shiny::plotOutput()] (`ggplot_output`) or [shiny::renderPlot()] (`render_ggplot`).
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
  tagDownload <- if (!is.null(downloads)) {
    e <- downloads[-1]
    e <- e[-length(e)]
    download_links <- lapply(
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
        class = "btn-sm esquisse-export-btn btn-outline-primary",
        style = css(
          position = "absolute",
          top = 0,
          right = "5px",
          zIndex = 30
        )
      ),
      placement = "bottom-end",
      download_links,
      if (!is.null(downloads$more)) {
        tagList(
          tags$hr(style = "margin: 5px 0;"),
          actionLink(inputId = ns("more"), label = downloads$more)
        )
      }
    )
  }
  tagPlot <- if (requireNamespace(package = "plotly")) {
    bslib::navset_hidden(
      id = ns("type_output"),
      selected = "plot",
      bslib::nav_panel(
        title = "plot",
        tags$div(
          id = ns("ggplot-container"),
          class = "ggplot-container",
          style = css(
            position = "relative",
            width = validateCssUnit(width),
            height = validateCssUnit(height)
          ),
          plotOutput(outputId = ns("plot"), width = "100%", height = "100%", ...)
        )
      ),
      bslib::nav_panel(
        title = "plotly",
        tags$div(
          id = ns("ggplotly-container"),
          class = "ggplotly-container",
          style = css(
            position = "relative",
            width = validateCssUnit(width),
            height = validateCssUnit(height)
          ),
          plotly::plotlyOutput(outputId = ns("plotly"), width = "100%", height = height, ...)
        )
      )
    )
  } else {
    tags$div(
      id = ns("ggplot-container"),
      class = "ggplot-container",
      style = css(
        position = "relative",
        width = validateCssUnit(width),
        height = validateCssUnit(height)
      ),
      plotOutput(outputId = ns("plot"), width = "100%", height = height, ...)
    )
  }
  tagList(
    html_dependency_moveable(),
    tagDownload,
    tagPlot,
    tags$div(
      style = "display: none;",
      textInput(inputId = ns("hidden"), label = NULL, value = genId())
    )
  )
}

#' @param label Main label for export button
#' @param png,pdf,svg,jpeg,pptx Labels to display in
#'  export menu, use \code{NULL} to disable specific format.
#' @param more Label for "more" button, allowing to launch export modal.
#'
#' @rdname ggplot-output
#' @export
downloads_labels <- function(label = ph("download-simple"),
                             png = tagList(ph("image"), "PNG"),
                             pdf = tagList(ph("file-pdf"), "PDF"),
                             svg = tagList(ph("browsers"), "SVG"),
                             jpeg = tagList(ph("image"), "JPEG"),
                             pptx = tagList(ph("projector-screen"), "PPTX"),
                             more = tagList(ph("gear"), i18n("More options"))) {
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
#' @param resizable Can the chart size be adjusted by the user?
#'
#' @rdname ggplot-output
#'
#' @export
#'
#' @importFrom shiny exprToFunction moduleServer downloadHandler
#'  reactiveValues renderPlot observeEvent showNotification is.reactive bindEvent
#' @importFrom shinyWidgets hideDropMenu
render_ggplot <- function(id,
                          expr,
                          ...,
                          env = parent.frame(),
                          quoted = FALSE,
                          filename = "export-ggplot",
                          resizable = FALSE,
                          use_plotly = reactive(FALSE),
                          width = reactive(NULL),
                          height = reactive(NULL)) {
  stopifnot("width must be a reactive function" = is.reactive(width))
  stopifnot("height must be a reactive function" = is.reactive(height))
  gg_fun <- exprToFunction(expr, env, quoted)
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      plot_width <- paste0("output_", ns("plot"), "_width")
      plot_height <- paste0("output_", ns("plot"), "_height")
      
      observeEvent(input$hidden, {
        if (isTRUE(resizable)) 
          activate_resizer(id = ns("ggplot-container"), modal = FALSE)
      })
      
      bindEvent(
        observe({
          if (
            isTruthy(width()) & isTruthy(height())
          ) {
            resize(
              id = ns("ggplot-container"),
              width = width(),
              height = height(),
              with_moveable = resizable
            )
          }
        }),
        width(),
        height()
      )
      
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
                ui = i18n("Export to PowerPoint failed..."),
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
      if (requireNamespace(package = "plotly")) {
        output$plotly <- plotly::renderPlotly({
          rv$plot <- gg_fun()
          rv$plot
        })
        observeEvent(use_plotly(), {
          if (isTRUE(use_plotly())) {
            bslib::nav_select(id = "type_output", selected = "plotly")
          } else {
            bslib::nav_select(id = "type_output", selected = "plot")
          }
        })
      }
      observeEvent(input$more, {
        hideDropMenu("exports_dropmenu")
        save_ggplot_modal(
          id = session$ns("export"),
          title = i18n("Export chart")
        )
      })
      save_ggplot_server("export", plot_rv = rv)
      observe({
        rv$plot_width <- session$clientData[[plot_width]]
      })
      observe({
        rv$plot_height <- session$clientData[[plot_height]]
      })
      return(rv)
    }
  )
}





# Utils download handlers -------------------------------------------------


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
#' @importFrom grDevices cairo_pdf
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
      if (identical(device, "pdf") && isTRUE(capabilities("cairo")))
        device <- grDevices::cairo_pdf
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

