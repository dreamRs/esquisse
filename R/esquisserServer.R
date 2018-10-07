#' Server for addin esquisser
#'
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param data    a \code{reactiveValues} with at least a slot \code{data} containing a \code{data.frame}
#'  to use in the module. And a slot \code{name} corresponding to the name of the \code{data.frame}.
#'
#' @noRd
#'
#' @importFrom shiny callModule reactiveValues observeEvent renderPrint renderPlot stopApp plotOutput showNotification
#'
esquisserServer <- function(input, output, session, data = NULL) {
  
  observeEvent(data$data, {
    dataChart$data <- data$data
    dataChart$name <- data$name
  })

  esquisse.env <- get("esquisse.env", envir = parent.env(environment()))
  dataChart <- callModule(
    module = chooseDataServer, 
    id = "choose-data",
    data = esquisse.env$data,
    name = esquisse.env$data_name,
    launchOnStart = is.null(esquisse.env$data),
    coerceVars = getOption(x = "esquisse.coerceVars", default = FALSE)
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
  shiny::observeEvent(list(input$dragvars$target, geomSelected$x), {
    types <- possible_geom(data = dataChart$data, x = input$dragvars$target$xvar, y = input$dragvars$target$yvar)
    geom_possible$x <- c("auto", types)

    if ("bar" %in% types & geomSelected$x %in% c("auto", "bar")) {
      geom_controls$x <- "bar"
    } else if ("histogram" %in% types & geomSelected$x %in% c("auto", "histogram")) {
      geom_controls$x <- "histogram"
    } else if ("density" %in% types & geomSelected$x %in% c("auto", "density")) {
      geom_controls$x <- "density"
    } else if ("point" %in% types & geomSelected$x %in% c("auto", "point")) {
      geom_controls$x <- "point"
    } else if ("line" %in% types & geomSelected$x %in% c("auto", "line")) {
      geom_controls$x <- "line"
    } else if ("violin" %in% types & geomSelected$x %in% c("violin")) {
      geom_controls$x <- "violin"
    } else {
      geom_controls$x <- "auto"
    }
    
    if (!is.null(input$dragvars$target$fill) | !is.null(input$dragvars$target$color)) {
      geom_controls$palette <- TRUE
    } else {
      geom_controls$palette <- FALSE
    }
  })

  # Module chart controls : title, xalabs, colors, export...
  paramsChart <- shiny::callModule(
    module = chartControlsServer, 
    id = "controls", 
    type = geom_controls, 
    data = reactive(dataChart$data)
  )
  # observeEvent(paramsChart$index, {
  #   dataChart$data_filtered <- dataChart$data[paramsChart$index, ]
  # })

  # Module to choose type of charts
  geomSelected <- shiny::callModule(
    module = imageButtonServer, id = "geom", default = "auto",
    img_ref = geom_icon_href(), enabled = geom_possible, selected = geom_possible
  )


  # aesthetics from drag-and-drop
  aes_r <- reactiveValues(x = NULL, y = NULL, fill = NULL, color = NULL, size = NULL)
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
  
  # i <- 0
  output$plooooooot <- renderPlot({
    req(dataChart$data); req(paramsChart$index); req(paramsChart$inputs); req(geomSelected$x)
    # i <<- i+1
    # print(paste("EXECUTED", i))
    data <- dataChart$data
    
    if (!is.null(paramsChart$index) && is.logical(paramsChart$index)) {
      data <- data[paramsChart$index, , drop = FALSE]
    }
    
    res <- tryCatch({
      gg <- ggtry(
        data = data,
        x = aes_r$x,
        y = aes_r$y,
        fill = aes_r$fill,
        color = aes_r$color,
        size = aes_r$size,
        params = reactiveValuesToList(paramsChart)$inputs,
        type = geomSelected$x
      )
      list(status = TRUE, gg = gg)
    }
    , error = function(e) {
      list(status = FALSE, gg = NULL, e = e)
    })
    
    if (!res$status) {
      showNotification(ui = res$e$message, type = "error")
    }
    
    res$gg

  })


  # Export PowerPoint
  observeEvent(paramsChart$inputs$export_ppt, {
    if (requireNamespace(package = "rvg") & requireNamespace(package = "officer")) {
      data <- dataChart$data
      if (!is.null(paramsChart$index) && is.logical(paramsChart$index)) {
        data <- data[paramsChart$index, ]
      }
      gg <- ggtry(
        data = data,
        x = input$dragvars$target$xvar,
        y = input$dragvars$target$yvar,
        fill = input$dragvars$target$fill,
        color = input$dragvars$target$color,
        size = input$dragvars$target$size,
        params = reactiveValuesToList(paramsChart)$inputs,
        type = geomSelected$x
      )
      ppt <- officer::read_pptx()
      ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
      ppt <- rvg::ph_with_vg(ppt, print(gg), type = "body")
      tmp <- tempfile(pattern = "charter", fileext = ".pptx")
      print(ppt, target = tmp)
      utils::browseURL(url = tmp)
    } else {
      warn <- "Packages 'officer' and 'rvg' are required to use this functionality."
      warning(warn, call. = FALSE)
      shiny::showNotification(ui = warn, type = "warning")
    }
  })

  # Code
  callModule(
    moduleCodeServer, id = "code",
    varSelected = input$dragvars$target,
    dataChart = dataChart,
    paramsChart = paramsChart,
    geomSelected = geomSelected
  )

  # Close addin
  observeEvent(input$close, shiny::stopApp())

}

