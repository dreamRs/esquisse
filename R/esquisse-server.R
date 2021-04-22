
#' @param data_rv A `reactiveValues` with at least a slot `data` containing a `data.frame`
#'  to use in the module. And a slot `name` corresponding to the name of the `data.frame`.
#' @param default_aes Default aesthetics to be used, can be a `character`
#'  vector or `reactive` function returning one.
#' @param import_from From where to import data, argument passed
#'  to \code{\link[datamods:import-modal]{datamods::import_ui}}.
#'
#' @export
#'
#' @rdname esquisse-module
#' @order 2
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent is.reactive
#'  renderPlot stopApp plotOutput showNotification isolate reactiveValuesToList
#' @importFrom ggplot2 ggplot_build ggsave %+%
#' @import ggplot2
#' @importFrom datamods import_modal import_server show_data
#' @importFrom rlang expr sym
esquisse_server <- function(id, 
                            data_rv = NULL,
                            default_aes = c("fill", "color", "size", "group", "facet"),
                            import_from = c("env", "file", "copypaste", "googlesheets")) {
  
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      ggplotCall <- reactiveValues(code = "")
      data_chart <- reactiveValues(data = NULL, name = NULL)
      
      # Settings modal (aesthetics choices)
      observeEvent(input$settings, {
        showModal(modal_settings(aesthetics = input$aesthetics))
      })
      
      # Generate drag-and-drop input
      output$ui_aesthetics <- renderUI({
        if (is.reactive(default_aes)) {
          aesthetics <- default_aes()
        } else {
          if (is.null(input$aesthetics)) {
            aesthetics <- default_aes
          } else {
            aesthetics <- input$aesthetics
          }
        }
        data <- isolate(data_chart$data)
        if (!is.null(data)) {
          var_choices <- get_col_names(data)
          dragulaInput(
            inputId = ns("dragvars"),
            sourceLabel = "Variables",
            targetsLabels = c("X", "Y", aesthetics),
            targetsIds = c("xvar", "yvar", aesthetics),
            choiceValues = var_choices,
            choiceNames = badgeType(
              col_name = var_choices,
              col_type = col_type(data[, var_choices, drop = TRUE])
            ),
            selected = dropNulls(isolate(input$dragvars$target)),
            badge = FALSE,
            width = "100%",
            height = "70px",
            replace = TRUE
          )
        } else {
          dragulaInput(
            inputId = ns("dragvars"),
            sourceLabel = "Variables",
            targetsLabels = c("X", "Y", aesthetics),
            targetsIds = c("xvar", "yvar", aesthetics),
            choices = "",
            badge = FALSE,
            width = "100%",
            height = "70px",
            replace = TRUE
          )
        }
      })
      
      observeEvent(data_rv$data, {
        data_chart$data <- data_rv$data
        data_chart$name <- data_rv$name
      }, ignoreInit = FALSE)
      
      # Launch import modal if no data at start
      if (is.null(isolate(data_rv$data))) {
        datamods::import_modal(
          id = ns("import-data"),
          from = import_from,
          title = "Import data to create a graph"
        )
      }
      
      # Launch import modal if button clicked
      observeEvent(input$launch_import_data, {
        datamods::import_modal(
          id = ns("import-data"),
          from = import_from,
          title = "Import data to create a graph"
        )
      })
      
      # Data imported and update rv used
      data_imported_r <- datamods::import_server("import-data", return_class = "tbl_df")
      observeEvent(data_imported_r$data(), {
        data <- data_imported_r$data()
        data_chart$data <- data
        data_chart$name <- data_imported_r$name() %||% "imported_data"
      })
      
      observeEvent(input$show_data, {
        data <- controls_rv$data
        if (!is.data.frame(data)) {
          showNotification(
            ui = "No data to display",
            duration = 700,
            id = paste("esquisse", sample.int(1e6, 1), sep = "-"),
            type = "warning"
          )
        } else {
          datamods::show_data(data, title = "Dataset", type = "modal")
        }
      })
      
      # Update drag-and-drop input when data changes
      observeEvent(data_chart$data, {
        data <- data_chart$data
        if (is.null(data)) {
          updateDragulaInput(
            session = session,
            inputId = "dragvars",
            status = NULL,
            choices = character(0),
            badge = FALSE
          )
        } else {
          # special case: geom_sf
          if (inherits(data, what = "sf")) {
            geom_possible$x <- c("sf", geom_possible$x)
          }
          var_choices <- get_col_names(data)
          updateDragulaInput(
            session = session,
            inputId = "dragvars",
            status = NULL,
            choiceValues = var_choices,
            choiceNames = badgeType(
              col_name = var_choices,
              col_type = col_type(data[, var_choices, drop = TRUE])
            ),
            badge = FALSE
          )
        }
      }, ignoreNULL = FALSE)
      
      geom_possible <- reactiveValues(x = "auto")
      geom_controls <- reactiveValues(x = "auto")
      observeEvent(list(input$dragvars$target, input$geom), {
        geoms <- potential_geoms(
          data = data_chart$data,
          mapping = build_aes(
            data = data_chart$data,
            x = input$dragvars$target$xvar,
            y = input$dragvars$target$yvar
          )
        )
        geom_possible$x <- c("auto", geoms)
        
        geom_controls$x <- select_geom_controls(input$geom, geoms)
        
        if (!is.null(input$dragvars$target$fill) | !is.null(input$dragvars$target$color)) {
          geom_controls$palette <- TRUE
        } else {
          geom_controls$palette <- FALSE
        }
      }, ignoreInit = TRUE)
      
      observeEvent(geom_possible$x, {
        geoms <- c(
          "auto", "line", "area", "bar", "histogram",
          "point", "boxplot", "violin", "density",
          "tile", "sf"
        )
        updateDropInput(
          session = session,
          inputId = "geom",
          selected = setdiff(geom_possible$x, "auto")[1],
          disabled = setdiff(geoms, geom_possible$x)
        )
      })
      
      # Module chart controls : title, xlabs, colors, export...
      # paramsChart <- reactiveValues(inputs = NULL)
      controls_rv <- controls_server(
        id = "controls",
        type = geom_controls,
        data_table = reactive(data_chart$data),
        data_name = reactive({
          req(data_chart$name)
          data_chart$name
        }),
        ggplot_rv = ggplotCall,
        aesthetics = reactive({
          dropNullsOrEmpty(input$dragvars$target)
        }),
        use_facet = reactive({
          !is.null(input$dragvars$target$facet) | !is.null(input$dragvars$target$facet_row) | !is.null(input$dragvars$target$facet_col)
        }),
        use_transX = reactive({
          if (is.null(input$dragvars$target$xvar))
            return(FALSE)
          identical(
            x = col_type(data_chart$data[[input$dragvars$target$xvar]]),
            y = "continuous"
          )
        }),
        use_transY = reactive({
          if (is.null(input$dragvars$target$yvar))
            return(FALSE)
          identical(
            x = col_type(data_chart$data[[input$dragvars$target$yvar]]),
            y = "continuous"
          )
        })
      )
      
      
      render_ggplot("plooooooot", {
        req(data_chart$data)
        req(controls_rv$data)
        req(controls_rv$inputs)
        req(input$geom)
        
        aes_input <- make_aes(input$dragvars$target)
        
        req(unlist(aes_input) %in% names(data_chart$data))
        
        mapping <- build_aes(
          data = data_chart$data,
          .list = aes_input,
          geom = input$geom
        )
        
        geoms <- potential_geoms(
          data = data_chart$data,
          mapping = mapping
        )
        req(input$geom %in% geoms)
        
        data <- controls_rv$data
        
        scales <- which_pal_scale(
          mapping = mapping,
          palette = controls_rv$colors$colors,
          data = data,
          reverse = controls_rv$colors$reverse
        )
        
        if (identical(input$geom, "auto")) {
          geom <- "blank"
        } else {
          geom <- input$geom
        }
        
        geom_args <- match_geom_args(input$geom, controls_rv$inputs, mapping = mapping)
        
        if (isTRUE(controls_rv$smooth$add) & input$geom %in% c("point", "line")) {
          geom <- c(geom, "smooth")
          geom_args <- c(
            setNames(list(geom_args), input$geom),
            list(smooth = controls_rv$smooth$args)
          )
        }
        if (!is.null(aes_input$ymin) & !is.null(aes_input$ymax) & input$geom %in% c("line")) {
          geom <- c("ribbon", geom)
          mapping_ribbon <- aes_input[c("ymin", "ymax")]
          geom_args <- c(
            list(ribbon = list(
              mapping = expr(aes(!!!syms2(mapping_ribbon))), 
              fill = controls_rv$inputs$color_ribbon
            )),
            setNames(list(geom_args), input$geom)
          )
          mapping$ymin <- NULL
          mapping$ymax <- NULL
        }
        
        scales_args <- scales$args
        scales <- scales$scales
        
        if (isTRUE(controls_rv$transX$use)) {
          scales <- c(scales, "x_continuous")
          scales_args <- c(scales_args, list(x_continuous = controls_rv$transX$args))
        }
        
        if (isTRUE(controls_rv$transY$use)) {
          scales <- c(scales, "y_continuous")
          scales_args <- c(scales_args, list(y_continuous = controls_rv$transY$args))
        }
        
        if (isTRUE(controls_rv$limits$x)) {
          xlim <- controls_rv$limits$xlim
        } else {
          xlim <- NULL
        }
        if (isTRUE(controls_rv$limits$y)) {
          ylim <- controls_rv$limits$ylim
        } else {
          ylim <- NULL
        }
        data_name <- data_chart$name %||% "data"
        gg_call <- ggcall(
          data = data_name,
          mapping = mapping,
          geom = geom,
          geom_args = geom_args,
          scales = scales,
          scales_args = scales_args,
          labs = controls_rv$labs,
          theme = controls_rv$theme$theme,
          theme_args = controls_rv$theme$args,
          coord = controls_rv$coord,
          facet = input$dragvars$target$facet,
          facet_row = input$dragvars$target$facet_row,
          facet_col = input$dragvars$target$facet_col,
          facet_args = controls_rv$facet,
          xlim = xlim,
          ylim = ylim
        )
        
        ggplotCall$code <- deparse2(gg_call)
        ggplotCall$call <- gg_call

        ggplotCall$ggobj <- safe_ggplot(
          expr = expr((!!gg_call) %+% !!sym("esquisse_data")),
          data = setNames(list(data, data), c("esquisse_data", data_chart$name))
        )
        ggplotCall$ggobj$plot
      }, filename = "esquisse-plot")
      
      
      # Close addin
      observeEvent(input$close, shiny::stopApp())
      
      # Ouput of module (if used in Shiny)
      output_module <- reactiveValues(code_plot = NULL, code_filters = NULL, data = NULL)
      observeEvent(ggplotCall$code, {
        output_module$code_plot <- ggplotCall$code
      }, ignoreInit = TRUE)
      observeEvent(controls_rv$data, {
        output_module$code_filters <- controls_rv$code
        output_module$data <- controls_rv$data
      }, ignoreInit = TRUE)
      
      return(output_module)
    }
  )
  
}
