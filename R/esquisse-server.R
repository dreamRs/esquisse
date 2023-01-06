
#' @param data_rv Either:
#'  * A [shiny::reactiveValues()] with a slot `data` containing a `data.frame` 
#'    to use in the module and a slot `name` corresponding to the name of the `data.frame` used for the generated code.
#'  * A [shiny::reactive()] function returning a `data.frame`. See argument `name` for the name used in generated code.
#'  * A `data.frame` object.
#' @param name The default name to use in generated code. Can be a `reactive` function return a single character.
#' @param default_aes Default aesthetics to be used, can be a `character`
#'  vector or `reactive` function returning one.
#' @param import_from From where to import data, argument passed
#'  to [datamods::import_server()], use `NULL` to prevent the modal to appear.
#' 
#'
#' @export
#'
#' @rdname esquisse-module
#' @order 2
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent is.reactive
#'  renderPlot stopApp plotOutput showNotification isolate reactiveValuesToList
#'  is.reactivevalues
#' @importFrom ggplot2 ggplot_build ggsave %+%
#' @import ggplot2
#' @importFrom datamods import_modal import_server show_data
#' @importFrom rlang expr sym
esquisse_server <- function(id,
                            data_rv = NULL,
                            name = "data",
                            default_aes = c("fill", "color", "size", "group", "facet"),
                            import_from = c("env", "file", "copypaste", "googlesheets", "url")) {

  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      ns <- session$ns
      ggplotCall <- reactiveValues(code = "")
      data_chart <- reactiveValues(data = NULL, name = NULL)
      geom_rv <- reactiveValues(possible = "auto", controls = "auto", palette = FALSE)

      # Settings modal (aesthetics choices)
      observeEvent(input$settings, {
        showModal(modal_settings(aesthetics = input$aesthetics))
      })

      
      if (is.reactivevalues(data_rv)) {
        observeEvent(data_rv$data, {
          data_chart$data <- data_rv$data
          data_chart$name <- data_rv$name %||% if (is.reactive(name)) {
            name()
          } else {
            name
          }
        }, ignoreInit = FALSE)
      } else if (is.reactive(data_rv)) {
        observeEvent(data_rv(), {
          data_chart$data <- data_rv()
          data_chart$name <- if (is.reactive(name)) {
            name()
          } else {
            name
          }
        }, ignoreInit = FALSE)
      } else if (is.data.frame(data_rv)) {
        data_chart$data <- as.data.frame(data_rv)
        data_chart$name <- if (is.character(name)) name
      }

      # Launch import modal if no data at start
      if (!is.null(import_from) & is.null(isolate(data_chart$data))) {
        datamods::import_modal(
          id = ns("import-data"),
          from = import_from,
          title = i18n("Import data to create a graph")
        )
      }

      # Launch import modal if button clicked
      observeEvent(input$launch_import_data, {
        datamods::import_modal(
          id = ns("import-data"),
          from = import_from,
          title = i18n("Import data to create a graph")
        )
      })

      # Data imported and update rv used
      data_imported_r <- datamods::import_server("import-data", return_class = "tbl_df")
      observeEvent(data_imported_r$data(), {
        data <- data_imported_r$data()
        data_chart$data <- data
        data_chart$name <- data_imported_r$name() %||% "imported_data"
      })

      # show data if button clicked
      show_data_server("show_data", reactive(controls_rv$data))
      
      # special case: geom_sf
      observeEvent(data_chart$data, {
        if (inherits(data_chart$data, what = "sf")) {
          geom_rv$possible <- c("sf", geom_rv$possible)
        }
      })
      
      # Aesthetic selector
      aes_r <- select_aes_server(
        id = "aes", 
        data_r = reactive(data_chart$data), 
        default_aes = default_aes,
        input_aes = reactive(input$aesthetics)
      )

      
      observeEvent(list(aes_r(), input$geom), {
        geoms <- potential_geoms(
          data = data_chart$data,
          mapping = build_aes(
            data = data_chart$data,
            x = aes_r()$xvar,
            y = aes_r()$yvar
          )
        )
        geom_rv$possible <- c("auto", geoms)

        geom_rv$controls <- select_geom_controls(input$geom, geoms)

        if (!is.null(aes_r()$fill) | !is.null(aes_r()$color)) {
          geom_rv$palette <- TRUE
        } else {
          geom_rv$palette <- FALSE
        }
      }, ignoreInit = TRUE)

      observeEvent(geom_rv$possible, {
        geoms <- c(
          "auto", "line", "area", "bar", "col", "histogram",
          "point", "jitter", "boxplot", "violin", "density",
          "tile", "sf"
        )
        updateDropInput(
          session = session,
          inputId = "geom",
          selected = setdiff(geom_rv$possible, "auto")[1],
          disabled = setdiff(geoms, geom_rv$possible)
        )
      })

      # Module chart controls : title, xlabs, colors, export...
      # paramsChart <- reactiveValues(inputs = NULL)
      controls_rv <- controls_server(
        id = "controls",
        type = geom_rv,
        data_table = reactive(data_chart$data),
        data_name = reactive({
          nm <- req(data_chart$name)
          if (is_call(nm)) {
            nm <- as_label(nm)
          }
          nm
        }),
        ggplot_rv = ggplotCall,
        aesthetics = reactive({
          dropNullsOrEmpty(aes_r())
        }),
        use_facet = reactive({
          !is.null(aes_r()$facet) | !is.null(aes_r()$facet_row) | !is.null(aes_r()$facet_col)
        }),
        use_transX = reactive({
          if (is.null(aes_r()$xvar))
            return(FALSE)
          identical(
            x = col_type(data_chart$data[[aes_r()$xvar]]),
            y = "continuous"
          )
        }),
        use_transY = reactive({
          if (is.null(aes_r()$yvar))
            return(FALSE)
          identical(
            x = col_type(data_chart$data[[aes_r()$yvar]]),
            y = "continuous"
          )
        })
      )


      render_ggplot("plooooooot", {
        req(input$play_plot, cancelOutput = TRUE)
        req(data_chart$data)
        req(controls_rv$data)
        req(controls_rv$inputs)
        req(input$geom)

        aes_input <- make_aes(aes_r())

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
        if (isTRUE(controls_rv$jitter$add) & input$geom %in% c("boxplot", "violin")) {
          geom <- c(geom, "jitter")
          geom_args <- c(
            setNames(list(geom_args), input$geom),
            list(jitter = controls_rv$jitter$args)
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
          facet = aes_r()$facet,
          facet_row = aes_r()$facet_row,
          facet_col = aes_r()$facet_col,
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
