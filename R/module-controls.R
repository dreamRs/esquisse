
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
                        controls = c("labs", "parameters", "appearance", "filters", "code"),
                        insert_code = FALSE,
                        layout = c("dropdown", "accordion"),
                        downloads = downloads_labels()) {
  ns <- NS(id)
  layout <- match.arg(layout)
  if (!is.null(controls)) {
    controls <- match.arg(
      controls,
      choices = c("labs", "parameters", "appearance", "filters", "code", "export"),
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
  if (isTRUE("parameters" %in% controls)) {
    listControls[[length(listControls) + 1]] <- funControl(
      controls_parameters_ui(ns("parameters")),
      inputId = ns("controls-parameters"),
      class = "esquisse-controls-parameters",
      style = "default",
      label = i18n("Plot options"),
      up = TRUE,
      icon = ph("gear"),
      status = "default btn-esquisse-controls btn-outline-primary text-nowrap"
    )
  }
  if (isTRUE("appearance" %in% controls)) {
    listControls[[length(listControls) + 1]] <- funControl(
      controls_appearance_ui(
        ns("appearance"),
        style = if (layout == "dropdown") {
          css(
            maxHeight = "80vh",
            overflowY = "auto",
            overflowX = "hidden",
            padding = "5px 7px"
          )
        }
      ),
      inputId = ns("controls-appearance"),
      class = "esquisse-controls-appearance",
      style = "default",
      label = i18n("Appearance"),
      up = TRUE,
      icon = ph("palette"),
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
#' @param type \code{reactiveValues} indicating the type of chart.
#' @param data_table \code{reactive} function returning data used in plot.
#' @param data_name \code{reactive} function returning data name.
#' @param ggplot_rv \code{reactiveValues} with ggplot object (for export).
#' @param aesthetics \code{reactive} function returning aesthetic names used.
#' @param use_facet \code{reactive} function returning
#'  \code{TRUE} / \code{FALSE} if plot use facets.
#' @param use_transX \code{reactive} function returning \code{TRUE} / \code{FALSE}
#'  to use transformation on x-axis.
#' @param use_transY \code{reactive} function returning \code{TRUE} / \code{FALSE}
#'  to use transformation on y-axis.
#'
#' @return A reactiveValues with all input's values
#' @noRd
#'
#' @importFrom shiny observeEvent reactiveValues reactiveValuesToList
#'  downloadHandler renderUI reactive updateTextInput showNotification callModule updateSliderInput debounce
#' @importFrom rstudioapi insertText getSourceEditorContext
#' @importFrom htmltools tags tagList
#' @importFrom datamods filter_data_server
#'
controls_server <- function(id,
                            type,
                            data_table,
                            data_name,
                            ggplot_rv,
                            aesthetics = reactive(NULL),
                            use_facet = reactive(FALSE),
                            use_transX = reactive(FALSE),
                            use_transY = reactive(FALSE),
                            width = reactive(NULL),
                            height = reactive(NULL),
                            drop_ids = TRUE) {

  callModule(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      labs_r <- controls_labs_server(
        id = "labs",
        data_table = data_table,
        aesthetics = aesthetics
      )

      appearance_r <- controls_appearance_server(
        id = "appearance",
        data_table = data_table,
        aesthetics = aesthetics,
        type = type
      )

      parameters_r <- controls_parameters_server(
        id = "parameters",
        use_facet = use_facet,
        use_transX = use_transX,
        use_transY = use_transY,
        width = width,
        height = height,
        type = type
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
          req(data_table())
          req(names(data_table()))
          if (isTRUE(input$disable_filters)) {
            return(NULL)
          } else {
            data_table()
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

      observeEvent(data_table(), {
        outputs$data <- data_table()
        outputs$code <- reactiveValues(expr = NULL, dplyr = NULL)
      })


      observeEvent(appearance_r$inputs(), {
        outputs$inputs <- modifyList(outputs$inputs, appearance_r$inputs())
      })

      observeEvent(parameters_r$inputs(), {
        outputs$inputs <- modifyList(outputs$inputs, parameters_r$inputs())
      })

      observeEvent(labs_r$labs(), {
        outputs$labs <- labs_r$labs()
      })


      observeEvent(appearance_r$colors(), {
        outputs$colors <- appearance_r$colors()
      })


      # theme input
      observe({
        theme_labs <- labs_r$theme()
        theme_appearance <- appearance_r$inputs()
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
              axis.text.y = theme_appearance$axis_text_y,
              axis.text.x = theme_appearance$axis_text_x,
              legend.text = theme_appearance$legend_text,
              legend.title = theme_appearance$legend_title
            )
          )
        )
      })

      # coord input
      observeEvent(parameters_r$coord(), {
        outputs$coord <- parameters_r$coord()
      }, ignoreNULL = FALSE)

      # smooth input
      observeEvent(parameters_r$smooth(), {
        outputs$smooth <- parameters_r$smooth()
      })

      # jittered input
      observeEvent(parameters_r$jitter(), {
        outputs$jitter <- parameters_r$jitter()
      })

      # transX input
      observeEvent(parameters_r$transX(), {
        outputs$transX <- parameters_r$transX()
      })

      # transY input
      observeEvent(parameters_r$transY(), {
        outputs$transY <- parameters_r$transY()
      })

      # facet input
      observeEvent(parameters_r$facet(), {
        outputs$facet <- parameters_r$facet()
      })

      # limits input
      observeEvent(parameters_r$limits(), {
        outputs$limits <- parameters_r$limits()
      })

      # width
      observeEvent(parameters_r$width(), {
        outputs$width <- parameters_r$width()
      })

      # height
      observeEvent(parameters_r$height(), {
        outputs$height <- parameters_r$height()
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


