
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
#' @param drop_ids Argument passed to [datamods::filter_data_server]. Drop columns containing more than 90% of unique values, or than 50 distinct values.
#' @param notify_warnings See [safe_ggplot()]. If `NULL`, the user can make his or her own choice via the settings menu, default is to show warnings once.
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
                            import_from = c("env", "file", "copypaste", "googlesheets", "url"),
                            drop_ids = TRUE,
                            notify_warnings = NULL) {

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
        data_chart$data <- data_rv
        data_chart$name <- if (is.character(name)) name
      }

      # Launch import modal if no data at start
      if (!is.null(import_from)) {
        observe({
          if (is.null(data_chart$data)) {
            datamods::import_modal(
              id = ns("import-data"),
              from = import_from,
              title = i18n("Import data to create a graph")
            )
          }
        })
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

      # update variable modal
      observeEvent(input$update_variable, {
        showModal(modalDialog(
          title = tagList(
            i18n("Update & select variables"),
            button_close_modal()
          ),
          datamods::update_variables_ui(ns("update_variable"), title = NULL),
          easyClose = TRUE,
          size = "l",
          footer = NULL
        ))
      })
      updated_data <-datamods::update_variables_server(
        id = "update_variable",
        data = reactive(data_chart$data)
      )
      observeEvent(updated_data(), {
        data_chart$data <- updated_data()
      })





      res_geom_aes_r <- select_geom_aes_server(
        id = "geomaes",
        data_r = reactive(data_chart$data),
        aesthetics_r = reactive(input$aesthetics),
        n_geoms = 5,
        default_aes = default_aes
      )
      aes_r <- reactive(res_geom_aes_r()$main$aes)
      aes_others_r <- reactive({
        others <- res_geom_aes_r()$others
        mappings <- others[grepl("aes", names(others))]
        lapply(
          X = mappings,
          FUN = function(x) {
            if (isTruthy(x)) {
              list(mapping = expr(aes(!!!syms2(make_aes(x)))))
            } else {
              NULL
            }
          }
        )
      })
      geom_r <- reactive(res_geom_aes_r()$main$geom)
      geoms_others_r <- reactive({
        others <- res_geom_aes_r()$others
        geoms <- others[grepl("geom", names(others))]
        unlist(geoms, use.names = FALSE)
      })

      # Module chart controls : title, xlabs, colors, export...
      # paramsChart <- reactiveValues(inputs = NULL)
      controls_rv <- controls_server(
        id = "controls",
        data_table = reactive(data_chart$data),
        data_name = reactive({
          nm <- req(data_chart$name)
          if (is_call(nm)) {
            nm <- as_label(nm)
          }
          nm
        }),
        ggplot_rv = ggplotCall,
        geoms_r = geom_r,
        aesthetics_r = reactive({
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
        }),
        width = reactive(rv_render_ggplot$plot_width),
        height = reactive(rv_render_ggplot$plot_height),
        drop_ids = drop_ids
      )


      rv_render_ggplot <- render_ggplot(
        id = "plooooooot",
        {
          req(input$play_plot, cancelOutput = TRUE)
          req(data_chart$data)
          req(controls_rv$data)
          req(controls_rv$inputs)
          geom_ <- req(geom_r())

          aes_input <- make_aes(aes_r())

          req(unlist(aes_input) %in% names(data_chart$data))

          mapping <- build_aes(
            data = data_chart$data,
            .list = aes_input,
            geom = geom_
          )

          geoms <- potential_geoms(
            data = data_chart$data,
            mapping = mapping
          )
          req(geom_ %in% geoms)

          data <- controls_rv$data

          scales <- which_pal_scale(
            mapping = mapping,
            palette = controls_rv$colors$colors,
            data = data,
            reverse = controls_rv$colors$reverse
          )

          if (identical(geom_, "auto")) {
            geom <- "blank"
          } else {
            geom <- geom_
          }

          geom_args <- match_geom_args(
            geom_,
            controls_rv$inputs,
            mapping = mapping,
            add_mapping = FALSE
          )

          if (isTruthy(geoms_others_r())) {
            geom <- c(geom, geoms_others_r())
            geom_args <- c(
              setNames(list(geom_args), geom_),
              aes_others_r()
            )
          }
          # if (isTRUE(controls_rv$jitter$add) & geom_ %in% c("boxplot", "violin")) {
          #   geom <- c(geom, "jitter")
          #   geom_args <- c(
          #     setNames(list(geom_args), geom_),
          #     list(jitter = controls_rv$jitter$args)
          #   )
          # }
          # if (!is.null(aes_input$ymin) & !is.null(aes_input$ymax) & geom_ %in% c("line")) {
          #   geom <- c("ribbon", geom)
          #   mapping_ribbon <- aes_input[c("ymin", "ymax")]
          #   geom_args <- c(
          #     list(ribbon = list(
          #       mapping = expr(aes(!!!syms2(mapping_ribbon))),
          #       fill = controls_rv$inputs$color_ribbon
          #     )),
          #     setNames(list(geom_args), geom_)
          #   )
          #   mapping$ymin <- NULL
          #   mapping$ymax <- NULL
          # }

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

          xlim <- if (isTRUE(controls_rv$limits$x)) {
            controls_rv$limits$xlim
          }
          ylim <- if (isTRUE(controls_rv$limits$y)) {
            controls_rv$limits$ylim
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
            data = setNames(list(data, data), c("esquisse_data", data_chart$name)),
            show_notification = notify_warnings %||% input$notify_warnings  %||% "once"
          )
          ggplotCall$ggobj$plot
        },
        filename = "esquisse-plot",
        width = reactive(controls_rv$width),
        height = reactive(controls_rv$height),
        use_plotly = reactive(controls_rv$plotly)
      )


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
