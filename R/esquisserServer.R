#' Server for addin esquisser
#'
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param data    a data.frame to use in the addin, if NULL a modal dialog is launched
#' to choose one from the user environment
#'
#' @return nothing
#' @noRd
#'
#' @importFrom shiny callModule reactiveValues observeEvent renderPrint renderPlot stopApp plotOutput
#'
esquisserServer <- function(input, output, session, data = NULL) {

  esquisse.env <- get("esquisse.env", envir = parent.env(environment()))
  dataChart <- callModule(
    module = chooseDataServer, 
    id = "choose-data",
    data = esquisse.env$data, name = esquisse.env$name,
    launchOnStart = is.null(esquisse.env$data)
  )
  observeEvent(dataChart$data, {
    # varSelected <- reactiveValues(x = NULL)
    updateDragulaInput(
      session = session, 
      inputId = "dragvars", status = NULL,
      choiceValues = names(dataChart$data), 
      choiceNames = badgeType(
        col_name = names(dataChart$data), 
        col_type = col_type(dataChart$data)
      )
    )
  })

  # varSelected <- shiny::callModule(dragAndDropServer, id = "dragvars", data = dataChart)

  geom_possible <- shiny::reactiveValues(x = "auto")
  geom_controls <- shiny::reactiveValues(x = "auto")
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
    img_ref = list(
      auto = "esquisse/geomIcon/gg-auto.png", line = "esquisse/geomIcon/gg-line.png",
      bar = "esquisse/geomIcon/gg-bar.png", histogram = "esquisse/geomIcon/gg-histo.png",
      point = "esquisse/geomIcon/gg-point.png", boxplot = "esquisse/geomIcon/gg-boxplot.png",
      density = "esquisse/geomIcon/gg-density.png", tile = "esquisse/geomIcon/gg-tile.png"
    ), enabled = geom_possible, selected = geom_possible
  )


  output$test <- shiny::renderPrint({
    # str(input$dragvars$target)
    # str(dataChart$data)
  })


  output$plooooooot <- shiny::renderPlot({
    data <- dataChart$data
    if (!is.null(paramsChart$index) && is.logical(paramsChart$index)) {
      data <- data[paramsChart$index, ]
    }
    vars <- input$dragvars$target
    vars <- unlist(vars$x, use.names = FALSE)
    if (all(vars %in% names(data))) {
      res <- tryCatch({
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
        list(status = TRUE, gg = gg)
      }
      , error = function(e) {
        list(status = FALSE, gg = NULL, e = e)
      })

      if (!res$status) {
        print(res$e)
      }

      res$gg
    }

  })


  # Export PowerPoint
  shiny::observeEvent(paramsChart$inputs$export_ppt, {
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
  })

  # Code
  shiny::callModule(
    moduleCodeServer, id = "code",
    varSelected = input$dragvars$target,
    dataChart = dataChart,
    paramsChart = paramsChart,
    geomSelected = geomSelected
  )

  # Close addin
  shiny::observeEvent(input$close, shiny::stopApp())

}

