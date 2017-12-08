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
  dataChart <- chooseDataServer(input, output, session, esquisse.env$data)
  shiny::observeEvent(input$changeData, {
    varSelected <- reactiveValues(x = NULL)
    dataChart$x <- NULL
    options("charter.ggbuilder.data" = NULL)
    dataChart <- chooseDataServer(input, output, session, data = NULL)
  }, ignoreInit = TRUE)
  observeEvent(dataChart$x, {
    varSelected <- reactiveValues(x = NULL)
  })

  varSelected <- shiny::callModule(dragAndDropServer, id = "dragvars", data = dataChart)

  geom_possible <- shiny::reactiveValues(x = "auto")
  geom_controls <- shiny::reactiveValues(x = "auto")
  shiny::observeEvent(list(varSelected$x, geomSelected$x), {
    types <- possible_geom(data = dataChart$x, x = varSelected$x$xvar, y = varSelected$x$yvar)
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
    
    if (!is.null(varSelected$x$fill) | !is.null(varSelected$x$color)) {
      geom_controls$palette <- TRUE
    } else {
      geom_controls$palette <- FALSE
    }
  })

  # Module chart controls : title, xalabs, colors, export...
  paramsChart <- shiny::callModule(chartControlsServer, id = "controls", type = geom_controls)

  # Module to choose type of charts
  geomSelected <- shiny::callModule(
    module = imageButtonServer, id = "geom", default = "auto",
    img_ref = list(
      auto = "esquisse/geomIcon/gg-auto.png", line = "esquisse/geomIcon/gg-line.png",
      bar = "esquisse/geomIcon/gg-bar.png", histogram = "esquisse/geomIcon/gg-histo.png",
      point = "esquisse/geomIcon/gg-point.png", boxplot = "esquisse/geomIcon/gg-boxplot.png",
      density = "esquisse/geomIcon/gg-density.png"
    ), enabled = geom_possible
  )


  output$test <- shiny::renderPrint({
    # str(varSelected$x)
    # str(dataChart$x)
  })


  output$plooooooot <- shiny::renderPlot({

    # str(dataChart$x)
    data <- dataChart$x
    vars <- reactiveValuesToList(varSelected)
    vars <- unlist(vars$x, use.names = FALSE)
    if (all(vars %in% names(data))) {
      res <- tryCatch({
        gg <- ggtry(
          data = dataChart$x,
          x = varSelected$x$xvar,
          y = varSelected$x$yvar,
          fill = varSelected$x$fill,
          color = varSelected$x$color,
          size = varSelected$x$size,
          params = reactiveValuesToList(paramsChart),
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

  output$plot_export <- shiny::renderPlot({

    # str(dataChart$x)
    # verif_params <<- reactiveValuesToList(varSelected)
    res <- tryCatch({
      gg <- ggtry(
        data = dataChart$x,
        x = varSelected$x$xvar,
        y = varSelected$x$yvar,
        fill = varSelected$x$fill,
        color = varSelected$x$color,
        size = varSelected$x$size,
        params = reactiveValuesToList(paramsChart),
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

  })

  # Export PNG
  shiny::observeEvent(paramsChart$export_png, {
    showModal(modalDialog(
      title = "Important message", size = "l",
      shiny::plotOutput(outputId = "plot_export")
    ))
  })

  # Export PowerPoint
  shiny::observeEvent(paramsChart$export_ppt, {
    gg <- ggtry(
      data = dataChart$x,
      x = varSelected$x$xvar,
      y = varSelected$x$yvar,
      fill = varSelected$x$fill,
      color = varSelected$x$color,
      size = varSelected$x$size,
      params = reactiveValuesToList(paramsChart),
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
    varSelected = varSelected,
    dataChart = dataChart,
    paramsChart = paramsChart,
    geomSelected = geomSelected
  )

  # Close addin
  shiny::observeEvent(input$close, shiny::stopApp())

}

