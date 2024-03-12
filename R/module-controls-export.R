
#' Controls for exporting plot
#'
#' Export plot in various format
#'
#' @param id Module ID.
#'
#' @noRd
#'
controls_export_ui <- function(id, downloads = downloads_labels()) {

  ns <- NS(id)

  e <- downloads[-1]
  e <- e[-length(e)]
  download_links <- lapply(
    X = seq_along(e),
    FUN = function(i) {
      if (is.null(e[[i]]))
        return(NULL)
      tagList(
        downloadButton(
          outputId = ns(paste0("export_", names(e)[i])),
          label = e[[i]],
          icon = NULL,
          class = "btn-outline-primary",
          style = css(width = "220px", margin = "2px auto")
        ),
        tags$br()
      )
    }
  )

  tags$div(
    style = css(textAlign = "center"),
    download_links,
    if (!is.null(downloads$more)) {
      tagList(
        tags$hr(style = "margin: 5px 0;"),
        actionLink(inputId = ns("more"), label = downloads$more)
      )
    }
  )
}



controls_export_server <- function(id,
                                   plot_r = reactive(NULL),
                                   width = reactive(868),
                                   height = reactive(400))  {
  moduleServer(
    id = id,
    function(input, output, session) {

      rv <- reactiveValues(plot = NULL)

      output$export_png <- download_plot_r(plot_r, "png", filename = "esquisse-plot", width = width, height = height)
      output$export_pdf <- download_plot_r(plot_r, "pdf", filename = "esquisse-plot", width = width, height = height)
      output$export_svg <- download_plot_r(plot_r, "svg", filename = "esquisse-plot", width = width, height = height)
      output$export_jpeg <- download_plot_r(plot_r, "jpeg", filename = "esquisse-plot", width = width, height = height)

      output$export_pptx <- downloadHandler(
        filename = "esquisse-plot.pptx",
        content = function(file) {
          if (requireNamespace(package = "rvg") & requireNamespace(package = "officer")) {
            gg <- plot_r()
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

      observeEvent(plot_r(), {
        rv$plot <- plot_r()
      })

      observeEvent(input$more, {
        hideDropMenu("exports_dropmenu")
        save_ggplot_modal(
          id = session$ns("export"),
          title = i18n("Export chart")
        )
      })
      save_ggplot_server("export", plot_rv = rv)

    }
  )
}



download_plot_r <- function(p_r = reactive(NULL), device, filename, width = reactive(868), height = reactive(400)) {
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
      if (identical(device, "pdf") && isTRUE(capabilities("cairo")))
        device <- grDevices::cairo_pdf
      ggsave(
        filename = file,
        plot = p_r()$plot,
        device = device,
        dpi = 72,
        width = width() / 72,
        height = height() / 72,
        scale = 1
      )
    }
  )
}

