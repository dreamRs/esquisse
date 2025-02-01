
#' @importFrom shinyWidgets dropdown
#' @importFrom htmltools tagAppendAttributes
dropdown_ <- function(..., class = NULL) {
  TAG <- tagAppendAttributes(
    dropdown(...),
    class = "btn-group-esquisse"
  )
  TAG$children[[1]]$attribs <- TAG$children[[1]]$attribs[-2]
  is_content <- grepl(pattern = "sw-dropdown-content", x = TAG$children[[2]]$attribs$class)
  if (!isTRUE(is_content))
    warning("Failed to add class to dropdown content", call. = FALSE)
  TAG$children[[2]] <- tagAppendAttributes(
    TAG$children[[2]],
    class = class
  )
  TAG
}

#' @importFrom bslib accordion_panel
#' @importFrom rlang have_name
accordion_panel_ <- function(..., label, icon) {
  args <- list(...)
  args <- args[!rlang::have_name(args)]
  bslib::accordion_panel(title = tagList(icon, tags$b(label)), value = label, !!!args)
}

# htmltools::tagAppendAttributes(
#   shinyWidgets::dropMenu(
#     actionButton(
#       inputId = "controls-appearance",
#       label = "Appearance",
#       icon = icon("palette"),
#       class = "btn-esquisse-controls btn-outline-primary"
#     ),
#     controls_appearance(ns),
#     placement = "top"
#   ),
#   class = "btn-group-esquisse"
# )


#' Controls menu
#'
#' @param id Module's ID. See \code{\link[shiny]{callModule}}.
#' @param controls Controls menu to be displayed. Use \code{NULL} to hide all menus.
#' @param insert_code Logical, Display or not a button to isert the ggplot
#'  code in the current user script (work only in RStudio).
#'
#' @return a \code{\link[shiny]{tagList}} containing UI elements
#' @noRd
#'
#' @importFrom htmltools tags tagList HTML
#' @importFrom shiny checkboxInput
#' @importFrom datamods filter_data_ui
#' @importFrom bslib accordion
controls_ui <- function(id,
                        controls = c("options", "labs", "axes", "geoms", "theme", "filters", "code"),
                        insert_code = FALSE,
                        layout = c("dropdown", "accordion"),
                        downloads = downloads_labels(),
                        n_geoms = 1) {
  ns <- NS(id)
  layout <- match.arg(layout)
  if (!is.null(controls)) {
    controls <- match.arg(
      controls,
      choices = c("options", "labs", "axes", "geoms", "theme", "filters", "code", "export"),
      several.ok = TRUE
    )
  } else {
    return(tags$div(
      style = "display: none;",
      checkboxInput(
        inputId = ns("disable_filters"),
        label = NULL,
        value = TRUE
      )
    ))
  }
  disable_filters <- !"filters" %in% controls
  if (isTRUE(disable_filters))
    controls <- setdiff(controls, "filters")

  funControl <- switch(
    layout,
    "dropdown" = dropdown_,
    "accordion" = accordion_panel_
  )
  containerControls <- switch(
    layout,
    "dropdown" = function(...) {
      tags$div(
        class = "btn-group-esquisse btn-group-justified-esquisse",
        ...
      )
    },
    "accordion" = function(...) {
      bslib::accordion(..., multiple = FALSE)
    }
  )

  listControls <- list()
  if (isTRUE("options" %in% controls)) {
    listControls[[length(listControls) + 1]] <- funControl(
      controls_options_ui(id = ns("options")),
      inputId = ns("controls-options"),
      class = "esquisse-controls-options",
      style = "default",
      label = i18n("Options"),
      up = TRUE,
      icon = ph("gear"),
      status = "default btn-esquisse-controls btn-outline-primary text-nowrap"
    )
  }
  if (isTRUE("labs" %in% controls)) {
    listControls[[length(listControls) + 1]] <- funControl(
      controls_labs_ui(id = ns("labs")),
      inputId = ns("controls-labs"),
      class = "esquisse-controls-labs",
      style = "default",
      label = i18n("Labels & Title"),
      up = TRUE,
      icon = ph("text-aa"),
      status = "default btn-esquisse-controls btn-outline-primary text-nowrap"
    )
  }
  if (isTRUE("axes" %in% controls)) {
    listControls[[length(listControls) + 1]] <- funControl(
      controls_axes_ui(ns("axes")),
      inputId = ns("controls-axes"),
      class = "esquisse-controls-axes",
      style = "default",
      label = i18n("Axes"),
      up = TRUE,
      icon = ph("vector-two"),
      status = "default btn-esquisse-controls btn-outline-primary text-nowrap"
    )
  }
  if (isTRUE("geoms" %in% controls)) {
    listControls[[length(listControls) + 1]] <- funControl(
      controls_multigeoms_ui(
        ns("geoms"),
        style = if (layout == "dropdown") {
          css(
            maxHeight = "80vh",
            overflowY = "auto",
            overflowX = "hidden",
            padding = "5px 7px"
          )
        },
        n_geoms = n_geoms
      ),
      inputId = ns("controls-geoms"),
      class = "esquisse-controls-geoms",
      style = "default",
      label = i18n("Geometries"),
      up = TRUE,
      icon = ph("wrench"),
      status = "default btn-esquisse-controls btn-outline-primary text-nowrap"
    )
  }
  if (isTRUE("theme" %in% controls)) {
    listControls[[length(listControls) + 1]] <- funControl(
      controls_theme_ui(
        ns("theme"),
        style = if (layout == "dropdown") {
          css(
            maxHeight = "80vh",
            overflowY = "auto",
            overflowX = "hidden",
            padding = "5px 7px"
          )
        }
      ),
      inputId = ns("controls-theme"),
      class = "esquisse-controls-theme",
      style = "default",
      label = i18n("Theme"),
      up = TRUE,
      icon = ph("paint-roller"),
      status = "default btn-esquisse-controls btn-outline-primary text-nowrap"
    )
  }
  if (isTRUE("filters" %in% controls)) {
    listControls[[length(listControls) + 1]] <- funControl(
      filter_data_ui(id = ns("filter-data")),
      inputId = ns("controls-filters"),
      class = "esquisse-controls-filters",
      style = "default",
      label = i18n("Data"),
      up = TRUE,
      icon = ph("sliders-horizontal"),
      status = "default btn-esquisse-controls btn-outline-primary text-nowrap"
    )
  }
  if (isTRUE("code" %in% controls)) {
    listControls[[length(listControls) + 1]] <- funControl(
      controls_code_ui(ns("code"), insert_code = insert_code),
      inputId = ns("controls-code"),
      class = "esquisse-controls-code",
      style = "default",
      label = i18n("Code"),
      up = TRUE,
      right = TRUE,
      icon = ph("code"),
      status = "default btn-esquisse-controls btn-outline-primary text-nowrap"
    )
  }
  if (isTRUE("export" %in% controls)) {
    listControls[[length(listControls) + 1]] <- funControl(
      controls_export_ui(ns("export"), downloads = downloads),
      inputId = ns("controls-export"),
      class = "esquisse-controls-export",
      style = "default",
      label = i18n("Export"),
      up = TRUE,
      right = TRUE,
      icon = ph("file-arrow-down"),
      status = "default btn-esquisse-controls btn-outline-primary text-nowrap"
    )
  }
  if (length(listControls) > 0) {
    listControls <- containerControls(!!!listControls)
  }
  tagList(
    listControls,
    tags$div(
      style = "display: none;",
      checkboxInput(
        inputId = ns("disable_filters"),
        label = NULL,
        value = isTRUE(disable_filters)
      )
    ),
    html_dependency_esquisse()
  )
}




#' Dropup buttons to hide chart's controls
#'
#' @param id Module's ID.
#' @param data_r `reactive` function returning data used in plot.
#' @param data_name `reactive` function returning data name.
#' @param ggplot_rv `reactiveValues` with ggplot object (for export).
#' @param aesthetics_r `reactive` function returning aesthetic names used.
#' @param geoms_r `reactive` function returning geoms selected.
#' @param active_geom_r `reactive` function returning the current geom.
#' @param n_geoms Number of geoms possible.
#' @param width,height Width and height of the chart.
#' @param drop_ids Drop or not IDs in filter module.
#'
#' @return A reactiveValues with all input's values
#' @noRd
#'
#' @importFrom shiny observeEvent reactiveValues reactiveValuesToList observe
#'  downloadHandler renderUI reactive updateTextInput showNotification callModule updateSliderInput debounce
#' @importFrom rstudioapi insertText getSourceEditorContext
#' @importFrom htmltools tags tagList
#' @importFrom datamods filter_data_server
#'
controls_server <- function(id,
                            data_r,
                            data_name,
                            ggplot_rv,
                            geoms_r = reactive(NULL),
                            active_geom_r = reactive("geom1"),
                            n_geoms = 1,
                            aesthetics_r = reactive(NULL),
                            width = reactive(NULL),
                            height = reactive(NULL),
                            drop_ids = TRUE) {

  callModule(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      options_r <- controls_options_server(
        id = "options",
        use_facet = reactive({
          aesth1 <- aesthetics_r()[[1]]
          !is.null(aesth1$facet) | !is.null(aesth1$facet_row) | !is.null(aesth1$facet_col)
        }),
        width = width,
        height = height
      )

      labs_r <- controls_labs_server(
        id = "labs",
        data_r = data_r,
        aesthetics_r = reactive(aesthetics_r()[[1]])
      )

      geometries_r <- controls_multigeoms_server(
        id = "geoms",
        data_r = data_r,
        aesthetics_r = aesthetics_r,
        geoms_r = geoms_r,
        n_geoms = n_geoms,
        active_geom_r = active_geom_r
      )

      theme_r <- controls_theme_server(
        id = "theme"
      )

      axes_r <- controls_axes_server(
        id = "axes",
        use_transX = reactive({
          data <- req(data_r())
          aes1 <- aesthetics_r()[[1]]
          if (is.null(aes1$xvar))
            return(FALSE)
          identical(
            x = col_type(data[[aes1$xvar]]),
            y = "continuous"
          )
        }),
        use_transY = reactive({
          data <- req(data_r())
          aes1 <- aesthetics_r()[[1]]
          if (is.null(aes1$yvar))
            return(FALSE)
          identical(
            x = col_type(data[[aes1$yvar]]),
            y = "continuous"
          )
        })
      )

      controls_export_server(
        id = "export",
        plot_r = reactive(ggplot_rv$ggobj),
        width = width,
        height = height
      )

      controls_code_server(
        id = "code",
        ggplot_rv = ggplot_rv,
        output_filter = output_filter,
        data_name = data_name
      )



      # Filter data module from datamods
      output_filter <- filter_data_server(
        id = "filter-data",
        data = reactive({
          req(data_r())
          req(names(data_r()))
          if (isTRUE(input$disable_filters)) {
            return(NULL)
          } else {
            data_r()
          }
        }),
        name = data_name,
        #########
        drop_ids = drop_ids
        ########
      )

      outputs <- reactiveValues(
        inputs = list(),
        export_ppt = NULL,
        export_png = NULL
      )

      observeEvent(data_r(), {
        outputs$data <- data_r()
        outputs$code <- data_name()
      })


      observeEvent(geometries_r(), {
        res <- geometries_r()
        lapply(
          X = seq_len(n_geoms),
          FUN = function(i) {
            outputs[[paste0("geomargs", i)]] <- res[[i]]$inputs
            outputs[[paste0("geomcolors", i)]] <- res[[i]]$colors
          }
        )
      })

      observeEvent(theme_r$inputs(), {
        outputs$inputs <- modifyList(outputs$inputs, theme_r$inputs())
      })

      observeEvent(axes_r$inputs(), {
        outputs$inputs <- modifyList(outputs$inputs, axes_r$inputs())
      })

      observeEvent(labs_r$labs(), {
        outputs$labs <- labs_r$labs()
      })


      # theme input
      observe({
        theme_labs <- labs_r$theme()
        theme_appearance <- theme_r$inputs()
        axes_appearance <- axes_r$inputs()
        outputs$theme <- list(
          theme = theme_appearance$theme,
          args = dropNulls(
            list(
              legend.position = theme_appearance$legend_position,
              legend.justification = theme_appearance$legend_justification,
              plot.title = theme_labs$title,
              plot.subtitle = theme_labs$subtitle,
              plot.caption = theme_labs$caption,
              axis.title.y = theme_labs$y,
              axis.title.x = theme_labs$x,
              axis.text.y = axes_appearance$axis_text_y,
              axis.text.x = axes_appearance$axis_text_x,
              legend.text = theme_appearance$legend_text,
              legend.title = theme_appearance$legend_title
            )
          )
        )
      })

      # coord input
      observeEvent(axes_r$coord(), {
        outputs$coord <- axes_r$coord()
      }, ignoreNULL = FALSE)

      # transX input
      observeEvent(axes_r$transX(), {
        outputs$transX <- axes_r$transX()
      })

      # transY input
      observeEvent(axes_r$transY(), {
        outputs$transY <- axes_r$transY()
      })

      # limits input
      observeEvent(axes_r$limits(), {
        outputs$limits <- axes_r$limits()
      })

      # facet input
      observeEvent(options_r$facet(), {
        outputs$facet <- options_r$facet()
      })

      # width
      observeEvent(options_r$width(), {
        outputs$width <- options_r$width()
      })

      # height
      observeEvent(options_r$height(), {
        outputs$height <- options_r$height()
      })

      # height
      observeEvent(options_r$plotly(), {
        outputs$plotly <- options_r$plotly()
      })

      observeEvent(output_filter$filtered(), {
        req(is.logical(input$disable_filters))
        if (!isTRUE(input$disable_filters)) {
          outputs$data <- output_filter$filtered()
          outputs$code <- output_filter$code()
        }
      })

      return(outputs)
    }
  )
}





select_geom_controls <- function(x, geoms) {
  if (length(x) < 1)
    return("auto")
  if ("bar" %in% geoms & x %in% c("auto", "bar", "col")) {
    "bar"
  } else if ("histogram" %in% geoms & x %in% c("auto", "histogram")) {
    "histogram"
  } else if ("density" %in% geoms & x %in% c("auto", "density")) {
    "density"
  } else if ("point" %in% geoms & x %in% c("auto", "point")) {
    "point"
  } else if ("line" %in% geoms & x %in% c("auto", "line", "step")) {
    "line"
  } else if ("area" %in% geoms & x %in% c("auto", "area")) {
    "area"
  } else if ("violin" %in% geoms & x %in% c("violin")) {
    "violin"
  } else if ("boxplot" %in% geoms & x %in% c("boxplot")) {
    "boxplot"
  } else if ("sf" %in% geoms & x %in% c("sf")) {
    "sf"
  } else {
    "auto"
  }
}


