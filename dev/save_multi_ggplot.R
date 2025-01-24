
library(shiny)
library(bslib)
library(phosphoricons)
library(ggplot2)
library(shinyWidgets)
library(rlang)

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
p4 <- ggplot(mtcars) + geom_bar(aes(carb))


plot_list_test <- list(
  list(ggobj = p1, code = "ggplot(mtcars) + geom_point(aes(mpg, disp))", label = "Plot 1"),
  list(ggobj = p2, code = "ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))", label = "Plot 2"),
  list(ggobj = p3, code = "ggplot(mtcars) + geom_smooth(aes(disp, qsec))", label = "Plot 3"),
  list(ggobj = p4, code = "ggplot(mtcars) + geom_bar(aes(carb))", label = "Plot 4")
)


card_plot <- function(id, obj) {
  tags$div(
    class = "col",
    tags$div(
      class = "card h-100",
      renderPlot(obj$ggobj),
      tags$div(
        class = "card-body",
        tags$h5(
          class = "card-title",
          obj$label
        ),
        tags$p(
          class = "card-text font-monospace bg-body-secondary rounded-2 p-1",
          obj$code
        )
      ),
      tags$div(
        class = "card-footer",
        htmltools::tagAppendAttributes(
          prettyToggle(
            inputId = id,
            value = TRUE,
            label_on = "Included in export",
            icon_on = icon("check"),
            status_on = "success",
            status_off = "danger",
            label_off = "Not included in export", 
            icon_off = icon("xmark"), 
            bigger = TRUE
          ),
          class = "my-2"
        )
      )
    )
  )
}

save_multi_ggplot_ui <- function(id, file_format = c("png", "pdf", "svg", "jpeg", "pptx")) {
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
    tags$div(
      class = "save-multi-ggplot-controls",
      downloadButton(
        outputId = ns("dl_code"),
        label = tagList(ph("code"), "Download code"),
        class = "btn-outline-primary",
        icon = NULL
      ),
      htmltools::tagAppendAttributes(
        dropMenu(
          actionButton(
            inputId = ns("dl_plots_drop"),
            label = tagList("Download plots", ph("caret-circle-down")),
            class = "btn-outline-primary",
          ),
          placement = "bottom-end",
          tags$div(
            style = htmltools::css(
              display = "grid",
              gridTemplateColumns = "repeat(2, 1fr)",
              gridGap = "10px"
            ),
            tags$div(
              class = "pe-2 border-end",
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
              )
            ),
            tags$div(
              download_links
            )
          )
        ),
        class = "d-inline-block"
      ),
      actionButton(
        inputId = ns("select_all"),
        label = tagList("(Un)select all"),
        class = "btn-outline-primary float-end"
      )
    ),
    tags$div(class = "clearfix"),
    uiOutput(
      outputId = ns("plots_container"),
      class = "row row-cols-md-3 mt-3"
    )
  )
}

save_multi_ggplot_server <- function(id,
                                     plot_list_r = reactive(NULL),
                                     filename_code = "code-ggplot.R",
                                     filename_zip = "ggplot.zip") {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
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
            card_plot(
              id = ns(paste0("include_plot_", i)),
              obj = plot_list[[i]]
            )
          }
        )
      })
      
      output$dl_code <- downloadHandler(
        filename = function() {
          if (is.reactive(filename_code))
            filename_code <- filename_code()
          filename_code
        }, 
        content = function(file) {
          plot_list <- plot_list_r()
          code_file <- tempfile(fileext = ".R")
          cat(
            "# Code ----\n\n\n",
            file = code_file
          )
          lapply(
            X = seq_along(plot_list),
            FUN = function(i) {
              if (!isTRUE(input[[paste0("include_plot_", i)]]))
                return(NULL)
              cat(
                sprintf("# %s ----\n\n", plot_list[[i]]$label),
                file = code_file,
                append = TRUE
              )
              cat(
                plot_list[[i]]$code,
                file = code_file,
                append = TRUE
              )
              cat(
                "\n\n\n",
                file = code_file,
                append = TRUE
              )
            }
          )
          file.copy(from = code_file, to = file)
        }
      )
      
      output$export_png <- download_multi_plot_handler(input, plot_list_r, "png", filename_zip)
      output$export_pdf <- download_multi_plot_handler(input, plot_list_r, "pdf", filename_zip)
      output$export_svg <- download_multi_plot_handler(input, plot_list_r, "svg", filename_zip)
      output$export_jpeg <- download_multi_plot_handler(input, plot_list_r, "jpeg", filename_zip)
      
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
                                        filename_zip = "ggplot.zip") {
  downloadHandler(
    filename = function() {
      if (is.reactive(filename_zip))
        filename_zip <- filename_zip()
      filename_zip
    },
    content = function(file) {
      plot_list <- plot_list_r()
      for (i in seq_along(plot_list)) {
        if (!isTRUE(input[[paste0("include_plot_", i)]]))
          plot_list[[i]] <- NULL
      }
      export_multi_ggplot(
        plot_list = plot_list_test, 
        device = device, 
        zipfile = file, 
        width = input$width, 
        height = input$height
      )
    }
  )
}


shinyApp(
  ui = page_fluid(
    save_multi_ggplot_ui("mod")
  ),
  server = function(...) {
    save_multi_ggplot_server(
      id = "mod", 
      plot_list_r = reactive(plot_list_test)
    )
  }
)

