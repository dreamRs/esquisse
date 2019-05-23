
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param data    a \code{reactiveValues} with at least a slot \code{data} containing a \code{data.frame}
#'  to use in the module. And a slot \code{name} corresponding to the name of the \code{data.frame}.
#' @param dataModule Data module to use, choose between \code{"GlobalEnv"}
#'  or \code{"ImportFile"}.
#' @param sizeDataModule Size for the modal window for selecting data.
#'
#' @export
#' 
#' @rdname esquisse-module
#'
#' @importFrom shiny callModule reactiveValues observeEvent renderPrint
#'  renderPlot stopApp plotOutput showNotification isolate
#' @importFrom ggplot2 ggplot_build ggsave
#'
esquisserServer <- function(input, output, session, data = NULL, dataModule = c("GlobalEnv", "ImportFile"), sizeDataModule = "m") {
  
  geomSelected <- reactiveValues(x = "auto")
  
  observeEvent(data$data, {
    dataChart$data <- data$data
    dataChart$name <- data$name
  }, ignoreInit = FALSE)

  dataChart <- callModule(
    module = chooseDataServer, 
    id = "choose-data",
    data = isolate(data$data),
    name = isolate(data$name),
    launchOnStart = is.null(isolate(data$data)),
    coerceVars = getOption(x = "esquisse.coerceVars", default = FALSE),
    dataModule = dataModule, size = sizeDataModule
  )
  observeEvent(dataChart$data, {
    # special case: geom_sf
    if (inherits(dataChart$data, what = "sf")) {
      geom_possible$x <- c("sf", geom_possible$x)
    } 
    var_choices <- setdiff(names(dataChart$data), attr(dataChart$data, "sf_column"))
    updateDragulaInput(
      session = session, 
      inputId = "dragvars", status = NULL,
      choiceValues = var_choices, 
      choiceNames = badgeType(
        col_name = var_choices, 
        col_type = col_type(dataChart$data[, var_choices])
      ),
      badge = FALSE
    )
  })

  geom_possible <- reactiveValues(x = "auto")
  geom_controls <- reactiveValues(x = "auto")
  shiny::observeEvent(list(input$dragvars$target, input$geom), {
    types <- possible_geom(data = dataChart$data, x = input$dragvars$target$xvar, y = input$dragvars$target$yvar)
    geom_possible$x <- c("auto", types)

    geom_controls$x <- select_geom_controls(input$geom, types)
    
    if (!is.null(input$dragvars$target$fill) | !is.null(input$dragvars$target$color)) {
      geom_controls$palette <- TRUE
    } else {
      geom_controls$palette <- FALSE
    }
  })
  
  observeEvent(input$geom, {
    geomSelected$x <- input$geom
  })
  observeEvent(geom_possible$x, {
    geoms <- c(
      "auto", "line", "area", "bar", "histogram", 
      "point", "boxplot", "violin", "density", 
      "tile", "sf"
    )
    # updateDropInput(session, "geom", selected = setdiff(geom_possible$x, "auto"))
    updateDropInput(
      session = session, 
      inputId = "geom",
      selected = setdiff(geom_possible$x, "auto")[1],
      disabled = setdiff(geoms, geom_possible$x)
    )
  })

  # Module chart controls : title, xalabs, colors, export...
  paramsChart <- reactiveValues(inputs = NULL)
  paramsChart <- callModule(
    module = chartControlsServer, 
    id = "controls", 
    type = geom_controls, 
    data = reactive(dataChart$data)
  )
  # observeEvent(paramsChart$index, {
  #   dataChart$data_filtered <- dataChart$data[paramsChart$index, ]
  # })


  # aesthetics from drag-and-drop
  aes_r <- reactiveValues(x = NULL, y = NULL, fill = NULL, color = NULL, size = NULL, group = NULL, facet = NULL)
  observeEvent(input$dragvars$target$xvar, {
    aes_r$x <- input$dragvars$target$xvar
  }, ignoreNULL = FALSE)
  observeEvent(input$dragvars$target$yvar, {
    aes_r$y <- input$dragvars$target$yvar
  }, ignoreNULL = FALSE)
  observeEvent(input$dragvars$target$fill, {
    aes_r$fill <- input$dragvars$target$fill
  }, ignoreNULL = FALSE)
  observeEvent(input$dragvars$target$color, {
    aes_r$color <- input$dragvars$target$color
  }, ignoreNULL = FALSE)
  observeEvent(input$dragvars$target$size, {
    aes_r$size <- input$dragvars$target$size
  }, ignoreNULL = FALSE)
  observeEvent(input$dragvars$target$group, {
    aes_r$group <- input$dragvars$target$group
  }, ignoreNULL = FALSE)
  observeEvent(input$dragvars$target$facet, {
    aes_r$facet <- input$dragvars$target$facet
  }, ignoreNULL = FALSE)
  
  
  # plot generated
  ggplot_r <- reactiveValues(p = NULL)
  
  i <- 0
  output$plooooooot <- renderPlot({
    req(input$play_plot, cancelOutput = TRUE)
    req(dataChart$data)
    req(paramsChart$index)
    req(paramsChart$inputs)
    req(input$geom)

    # i <<- i+1
    # print(paste("EXECUTED", i))
    
    data <- dataChart$data
    if (!is.null(paramsChart$index) && is.logical(paramsChart$index) & length(paramsChart$index) > 0) {
      data <- data[paramsChart$index, , drop = FALSE]
    }
    
    gg <- withCallingHandlers(
      expr = tryCatch(
        expr = {
          gg <- ggtry(
            data = data,
            x = aes_r$x,
            y = aes_r$y,
            fill = aes_r$fill,
            color = aes_r$color,
            size = aes_r$size,
            group = aes_r$group,
            facet = aes_r$facet,
            params = reactiveValuesToList(paramsChart)$inputs,
            type = input$geom
          )
          gg <- ggplot_build(gg)
          ggplot_r$p <- gg$plot
          print(gg$plot)
          gg
        },
        error = function(e) {
          showNotification(ui = conditionMessage(e), type = "error", session = session)
        }
      ), 
      warning = function(w) {
        showNotification(ui = conditionMessage(w), type = "warning", session = session)
      }
    )
  })


  # Export PowerPoint
  observeEvent(paramsChart$export_ppt, {
    if (requireNamespace(package = "rvg") & requireNamespace(package = "officer")) {
      gg <- ggplot_r$p
      ppt <- officer::read_pptx()
      ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
      ppt <- try(rvg::ph_with_vg(ppt, print(gg), type = "body"), silent = TRUE)
      if ("try-error" %in% class(ppt)) {
        shiny::showNotification(ui = "Export to PowerPoint failed...", type = "error")
      } else {
        tmp <- tempfile(pattern = "esquisse", fileext = ".pptx")
        print(ppt, target = tmp)
        utils::browseURL(url = tmp)
      }
    } else {
      warn <- "Packages 'officer' and 'rvg' are required to use this functionality."
      warning(warn, call. = FALSE)
      shiny::showNotification(ui = warn, type = "warning")
    }
  })
  
  # Export png
  observeEvent(paramsChart$export_png, {
    tmp <- tempfile(pattern = "esquisse", fileext = ".png")
    pngg <- try(ggsave(filename = tmp, plot = ggplot_r$p, width = 12, height = 8, dpi = "retina"))
    if ("try-error" %in% class(pngg)) {
      shiny::showNotification(ui = "Export to PNG failed...", type = "error")
    } else {
      utils::browseURL(url = tmp)
    }
  })

  # Code
  res_code <- callModule(
    moduleCodeServer, id = "controls-code",
    varSelected = aes_r,
    dataChart = dataChart,
    paramsChart = paramsChart,
    geomSelected = geomSelected
  )

  # Close addin
  observeEvent(input$close, shiny::stopApp())
  
  output_module <- reactiveValues(code = NULL, data = NULL)
  observeEvent(res_code(), {
    output_module$code <- res_code()
  }, ignoreInit = TRUE)
  observeEvent(list(dataChart$data, paramsChart$index), {
    data <- dataChart$data
    if (!is.null(paramsChart$index) && is.logical(paramsChart$index) & length(paramsChart$index) > 0) {
      data <- data[paramsChart$index, , drop = FALSE]
    }
    output_module$data <- data
  }, ignoreInit = TRUE)

  return(output_module)
}

