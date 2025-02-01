
library(shiny)
library(bslib)
library(phosphoricons)
library(ggplot2)
library(shinyWidgets)
library(rlang)
library(esquisse)

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
p4 <- ggplot(mtcars) + geom_bar(aes(carb))
p5 <- ggplot(presidential) +
  geom_segment(aes(y = name, x = start, xend = end)) +
  geom_point(aes(y = name, x = start)) +
  geom_point(aes(y = name, x = end))


plot_list_test <- list(
  list(ggobj = p1, code = "ggplot(mtcars) + geom_point(aes(mpg, disp))", label = "Plot 1"),
  list(ggobj = p2, code = "ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))", label = "Plot 2"),
  list(ggobj = p3, code = "ggplot(mtcars) + geom_smooth(aes(disp, qsec))", label = "Plot 3"),
  list(ggobj = p4, code = "ggplot(mtcars) + geom_bar(aes(carb))", label = "Plot 4"),
  list(ggobj = p5, code = "ggplot(presidential) +
  geom_segment(aes(y = name, x = start, xend = end)) +
  geom_point(aes(y = name, x = start)) +
  geom_point(aes(y = name, x = end))", label = "Plot 5")
)
for (i in seq_along(plot_list_test)) {
  plot_list_test[[i]]$code <- NULL
}

vapply(
  X = plot_list_test,
  FUN = function(x) {
    length(x$code) > 0
  },
  FUN.VALUE = logical(1)
)

export_multi_plot_card <- function(index,
                                   obj,
                                   export_btn_id = "export",
                                   ns = identity) {
  tags$div(
    class = "col mb-2",
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
        htmltools::tagAppendAttributes(
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

save_multi_ggplot_ui <- function(id,
                                 file_format = c("png", "pdf", "svg", "jpeg", "pptx")) {
  ns <- NS(id)
  file_format <- match.arg(
    arg = file_format,
    choices = c("png", "pdf", "svg", "jpeg", "pptx"),
    several.ok = TRUE
  )
  download_links <- lapply(
    X = seq_along(file_format),
    FUN = function(i) {
      tagList(
        downloadButton(
          outputId = ns(paste0("export_", file_format[i])),
          label = tagList(ph("download"), file_format[[i]]),
          class = "btn-outline-primary d-block my-1",
          icon = NULL,
          style = htmltools::css(width = "100%")
        )
      )
    }
  )

  tags$div(
    class = "save-multi-ggplot-container",
    card(
      fill = FALSE,
      layout_sidebar(
        uiOutput(
          outputId = ns("plots_container"),
          class = "row row-cols-md-3 mt-3"
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

save_multi_ggplot_server <- function(id,
                                     plot_list_r = reactive(NULL),
                                     filename = "code-ggplot") {
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
            paste_code(plot_list, .input = input),
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
          title = tagList("Code", esquisse:::button_close_modal()),
          footer = NULL,
          size = "l",
          easyClose = TRUE,
          HTML(downlit::highlight(
            paste_code(plot_list, .input = input),
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



paste_code <- function(plot_list, .input = list()) {
  Reduce(
    function(...) paste(..., sep = "\n\n\n"),
    esquisse:::dropNulls(lapply(
      X = seq_along(plot_list),
      FUN = function(index) {
        if (!isTRUE(.input[[paste0("include_plot_", index)]]))
          return(NULL)
        paste(
          sprintf("# %s ----\n", plot_list[[index]]$label %||% ""),
          plot_list[[index]]$code,
          sep = "\n"
        )
      }
    ))
  )
}



shinyApp(
  ui = page_fluid(
    theme = bs_theme_esquisse(),
    esquisse:::html_dependency_esquisse(),
    save_multi_ggplot_ui("mod")
  ),
  server = function(...) {
    save_multi_ggplot_server(
      id = "mod",
      plot_list_r = reactive(plot_list_test)
    )
  }
)

