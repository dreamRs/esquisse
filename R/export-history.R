
#' @title Save multiple `ggplot` module
#'
#' @description Save multiple `ggplot` objects in various format and retrieve code.
#'
#' @param id Module ID.
#' @param output_format Output formats offered to the user.
#'
#' @returns No value. Use in UI & server in shiny application.
#' @export
#'
#' @importFrom shiny downloadButton actionButton uiOutput
#' @importFrom htmltools tagList tags
#' @importFrom bslib card layout_sidebar sidebar
#' @importFrom shinyWidgets numericInputIcon
#'
#' @name save-ggplot-multi-module
#'
#' @example examples/save-ggplot-multi-module.R
save_multi_ggplot_ui <- function(id,
                                 output_format = c("png", "pdf", "svg", "jpeg", "pptx")) {
  ns <- NS(id)
  output_format <- match.arg(
    arg = output_format,
    choices = c("png", "pdf", "svg", "jpeg", "pptx"),
    several.ok = TRUE
  )
  download_links <- lapply(
    X = seq_along(output_format),
    FUN = function(i) {
      downloadButton(
        outputId = ns(paste0("export_", output_format[i])),
        label = tagList(ph("download"), output_format[[i]]),
        class = "btn-outline-primary d-block my-1 w-100",
        icon = NULL
      )
    }
  )

  tags$div(
    class = "save-multi-ggplot-container",
    html_dependency_esquisse(),
    card(
      fill = FALSE,
      layout_sidebar(
        uiOutput(
          outputId = ns("plots_container"),
          class = "row"
        ),
        sidebar = sidebar(
          position = "right",
          open = "always",
          tags$div(
            class = "save-multi-ggplot-controls",
            actionButton(
              inputId = ns("select_all"),
              label = tagList(ph("selection-inverse"), "(Un)select all"),
              class = "btn-outline-primary w-100 active",
              `data-bs-toggle` = "button",
              `aria-pressed` = "true"
            ),
            tags$hr(),
            downloadButton(
              outputId = ns("dl_code"),
              label = tagList(ph("code"), "Download code"),
              class = "btn-outline-primary w-100 mb-1",
              icon = NULL
            ),
            actionButton(
              inputId = ns("view_code"),
              label = tagList(ph("eye"), "View all code"),
              class = "btn-outline-primary w-100"
            ),
            tags$hr(),
            numericInputIcon(
              inputId = ns("width"),
              label = "Default width:",
              value = 868,
              icon = list(NULL, "px"),
              width = "100%"
            ),
            numericInputIcon(
              inputId = ns("height"),
              label = "Default height:",
              value = 400,
              icon = list(NULL, "px"),
              width = "100%"
            ),
            download_links
          )
        )
      )
    )
  )
}

#' @param plot_list_r A `reactive` function returning a list of plots and codes to export.
#'  Sub list items can have following names:
#'   * `ggobj`: the `ggplot` object producing the plot
#'   * `code`: code to produce the chart (optional)
#'   * `label`: a label to identify the plot
#' @param filename Name for the file exported.
#' @param placeholder A placeholder message to be displayed if `plot_list_r` return an empty list.
#' @param code_pre Some code to put before plots code.
#'
#' @export
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent renderUI downloadHandler showModal modalDialog
#'  showNotification
#' @importFrom shinyWidgets updatePrettyToggle
#' @importFrom htmltools HTML
#'
#' @rdname save-ggplot-multi-module
save_multi_ggplot_server <- function(id,
                                     plot_list_r = reactive(NULL),
                                     filename = "code-ggplot",
                                     placeholder = "No plots to display",
                                     code_pre = "library(ggplot2)") {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      rv <- reactiveValues()

      observeEvent(input$select_all, {
        plot_list <- plot_list_r()
        value <- isTRUE(input$select_all %% 2 == 0)
        lapply(
          X = seq_along(plot_list),
          FUN = function(i) {
            updatePrettyToggle(
              session = session,
              inputId = paste0("include_plot_", i),
              value = value
            )
          }
        )
      }, ignoreInit = TRUE)

      output$plots_container <- renderUI({
        plot_list <- plot_list_r()
        if (length(plot_list) < 1)
          return(placeholder)
        lapply(
          X = seq_along(plot_list),
          FUN = function(i) {
            export_multi_plot_card(
              index = i,
              obj = plot_list[[i]],
              ns = ns,
              export_btn_id = "export_plot"
            )
          }
        )
      })

      # Export individual plots
      observeEvent(input$export_plot, {
        plot_list <- plot_list_r()
        rv$plot <- plot_list[[input$export_plot]]$ggobj
        save_ggplot_modal(ns("export_plot"), "Export plot")
      })
      save_ggplot_server("export_plot", rv)


      # Donwload code
      output$dl_code <- downloadHandler(
        filename = function() {
          if (is.reactive(filename))
            filename <- filename()
          paste0(filename, ".R")
        },
        content = function(file) {
          plot_list <- plot_list_r()
          code_file <- tempfile(fileext = ".R")
          cat(
            "# esquisse code -------\n\n\n",
            file = code_file
          )
          cat(
            paste_code(plot_list, .input = input, code_pre = code_pre),
            file = code_file,
            append = TRUE
          )
          file.copy(from = code_file, to = file)
        }
      )

      # View code
      observeEvent(input$view_code, {
        plot_list <- plot_list_r()
        showModal(modalDialog(
          title = tagList("Code", button_close_modal()),
          footer = NULL,
          size = "l",
          easyClose = TRUE,
          HTML(downlit::highlight(
            paste_code(plot_list, .input = input, code_pre = code_pre),
            pre_class = "esquisse-code",
            code = TRUE,
            classes = downlit::classes_pandoc()
          ))
        ))
      })

      # Download multi plots
      output$export_png <- download_multi_plot_handler(input, plot_list_r, "png", filename)
      output$export_pdf <- download_multi_plot_handler(input, plot_list_r, "pdf", filename)
      output$export_svg <- download_multi_plot_handler(input, plot_list_r, "svg", filename)
      output$export_jpeg <- download_multi_plot_handler(input, plot_list_r, "jpeg", filename)

      output$export_pptx <- downloadHandler(
        filename = function() {
          if (is.reactive(filename))
            filename <- filename()
          paste0(filename, ".pptx")
        },
        content = function(file) {
          if (requireNamespace(package = "rvg") & requireNamespace(package = "officer")) {
            plot_list <- plot_list_r()
            ppt <- officer::read_pptx()
            ppt <- try({
              for (index in seq_along(plot_list)) {
                if (!isTRUE(input[[paste0("include_plot_", index)]]))
                  next
                ppt <- officer::add_slide(x = ppt, layout = "Blank")
                ppt <- officer::ph_with(
                  x = ppt,
                  value = rvg::dml(ggobj = plot_list[[index]]$ggobj),
                  location = officer::ph_location_fullsize()
                )
              }
              ppt
            }, silent = FALSE)
            if (inherits(ppt, "try-error")) {
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

    }
  )
}


#' @importFrom shiny renderPlot actionButton
#' @importFrom shinyWidgets prettyToggle dropMenu numericInputIcon
#' @importFrom htmltools tagAppendAttributes tags HTML tagList
#' @importFrom phosphoricons ph
export_multi_plot_card <- function(index,
                                   obj,
                                   export_btn_id = "export",
                                   ns = identity) {
  tags$div(
    class = "col-4 mb-2",
    tags$div(
      class = "card h-100",
      renderPlot(obj$ggobj),
      tags$div(
        class = "card-body",
        tags$h5(
          class = "card-title",
          obj$label
        ),
        if (!is.null(obj$code)) {
          HTML(downlit::highlight(
            obj$code,
            pre_class = "esquisse-code",
            code = TRUE,
            classes = downlit::classes_pandoc()
          ))
        }
      ),
      tags$div(
        class = "card-footer d-flex py-2",
        tagAppendAttributes(
          prettyToggle(
            inputId = ns(paste0("include_plot_", index)),
            value = TRUE,
            label_on = "Export",
            icon_on = icon("check"),
            status_on = "success",
            status_off = "danger",
            label_off = "Don't export",
            icon_off = icon("xmark"),
            bigger = TRUE,
            inline = TRUE
          ),
          class = "flex-grow-1 mb-0 mt-2"
        ),
        dropMenu(
          actionButton(
            inputId = ns(paste0("setting_plot_", index)),
            label = tagList(ph("gear", title = "Settings for this plot")),
            class = "btn-outline-primary me-2"
          ),
          numericInputIcon(
            inputId = ns(paste0("width_plot_", index)),
            label = "Width:",
            value = NA,
            icon = list(NULL, "px"),
            width = "100%"
          ),
          numericInputIcon(
            inputId = ns(paste0("height_plot_", index)),
            label = "Height:",
            value = NA,
            icon = list(NULL, "px"),
            width = "100%"
          )
        ),
        tags$button(
          type = "button",
          class = "btn btn-outline-primary",
          ph("download", title = "Export this plot"),
          onclick = sprintf(
            "Shiny.setInputValue('%s', %s, {priority: 'event'})",
            ns(export_btn_id), index
          )
        )
      )
    )
  )
}


#' @importFrom ggplot2 ggsave
#' @importFrom zip zip
export_multi_ggplot <- function(plot_list,
                                zipfile,
                                device = c("png", "pdf", "svg", "jpeg"),
                                width = 868,
                                height = 400) {
  device <- match.arg(device)
  plot_dir <- tempfile(pattern = "export_plot_dir")
  dir.create(plot_dir)
  for (obj in plot_list) {
    ggsave(
      path = plot_dir,
      filename = paste(obj$label, device, sep = "."),
      plot = obj$ggobj,
      device = device,
      dpi = 72,
      width = obj$width %||% width / 72,
      height = obj$height %||% height / 72,
      scale = 1
    )
  }
  zip::zip(
    zipfile = zipfile,
    files = list.files(plot_dir, full.names = TRUE),
    mode = "cherry-pick"
  )
}

#' @importFrom shiny downloadHandler isTruthy
download_multi_plot_handler <- function(input,
                                        plot_list_r,
                                        device,
                                        filename = "export-ggplot") {
  downloadHandler(
    filename = function() {
      if (is.reactive(filename))
        filename <- filename()
      paste0(filename, ".zip")
    },
    content = function(file) {
      plot_list <- plot_list_r()
      for (index in seq_along(plot_list)) {
        if (isTruthy(input[[paste0("height_plot_", index)]]))
          plot_list[[index]]$height <- input[[paste0("height_plot_", index)]]
        if (isTruthy(input[[paste0("width_plot_", index)]]))
          plot_list[[index]]$width <- input[[paste0("width_plot_", index)]]
      }
      for (index in seq_along(plot_list)) {
        if (!isTRUE(input[[paste0("include_plot_", index)]]))
          plot_list[[index]] <- NULL
      }
      export_multi_ggplot(
        plot_list = plot_list,
        device = device,
        zipfile = file,
        width = input$width,
        height = input$height
      )
    }
  )
}


#' @importFrom rlang %||%
paste_code <- function(plot_list, .input = list(), code_pre = "") {
  code <- Reduce(
    function(...) paste(..., sep = "\n\n\n"),
    dropNulls(lapply(
      X = seq_along(plot_list),
      FUN = function(index) {
        if (!isTRUE(.input[[paste0("include_plot_", index)]]))
          return(NULL)
        paste(
          sprintf("# %s ----\n", plot_list[[index]]$label %||% paste("Plot", index)),
          plot_list[[index]]$code,
          sep = "\n"
        )
      }
    ))
  )
  paste(code_pre, code, sep = "\n\n\n")
}



