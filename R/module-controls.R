
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
#'
controls_ui <- function(id,
                        controls = c("labs", "parameters", "appearance", "filters", "code"),
                        insert_code = FALSE) {
  ns <- NS(id)
  if (!is.null(controls)) {
    controls <- match.arg(
      controls,
      choices = c("labs", "parameters", "appearance", "filters", "code"),
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

  tagList(
    tags$div(
      class = "btn-group-esquisse btn-group-justified-esquisse",
      if (isTRUE("labs" %in% controls)) {
        dropdown_(
          controls_labs_ui(id = ns("labs")),
          inputId = ns("controls-labs"),
          class = "esquisse-controls-labs",
          style = "default",
          label = i18n("Labels & Title"),
          up = TRUE,
          icon = ph("text-aa"),
          status = "default btn-esquisse-controls btn-outline-primary text-nowrap"
        )
      },
      if (isTRUE("parameters" %in% controls)) {
        dropdown_(
          controls_parameters_ui(ns("parameters")),
          inputId = ns("controls-parameters"),
          class = "esquisse-controls-parameters",
          style = "default",
          label = i18n("Plot options"),
          up = TRUE,
          icon = ph("gear"),
          status = "default btn-esquisse-controls btn-outline-primary text-nowrap"
        )
      },
      if (isTRUE("appearance" %in% controls)) {
        dropdown_(
          controls_appearance_ui(ns("appearance")),
          inputId = ns("controls-appearance"),
          class = "esquisse-controls-appearance",
          style = "default",
          label = i18n("Appearance"),
          up = TRUE,
          icon = ph("palette"),
          status = "default btn-esquisse-controls btn-outline-primary text-nowrap"
        )
      },
      if (isTRUE("filters" %in% controls)) {
        dropdown_(
          filter_data_ui(id = ns("filter-data")),
          inputId = ns("controls-filters"),
          class = "esquisse-controls-filters",
          style = "default",
          label = i18n("Data"),
          up = TRUE,
          icon = ph("sliders-horizontal"),
          status = "default btn-esquisse-controls btn-outline-primary text-nowrap"
        )
      },
      if (isTRUE("code" %in% controls)) {
        dropdown_(
          controls_code(ns, insert_code = insert_code),
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
    ),
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
        type = type
      )

      # Code ----
      observeEvent(input$insert_code, {
        context <- rstudioapi::getSourceEditorContext()
        code <- ggplot_rv$code
        expr <- output_filter$expr()
        if (!is.null(expr) & !isTRUE(input$disable_filters)) {
          code_dplyr <- deparse2(output_filter$code())
          code_dplyr <- paste(code_dplyr, collapse = "\n")
          nm_dat <- data_name()
          code <- gsub(x = code, replacement = " ggplot()", pattern = sprintf("ggplot(%s)", nm_dat), fixed = TRUE)
          code <- paste(code_dplyr, code, sep = " %>%\n")
          if (input$insert_code == 1) {
            code <- paste("library(dplyr)\nlibrary(ggplot2)", code, sep = "\n\n")
          }
        } else {
          if (input$insert_code == 1) {
            code <- paste("library(ggplot2)", code, sep = "\n\n")
          }
        }
        rstudioapi::insertText(text = paste0("\n", code, "\n"), id = context$id)
      })

      output$code <- renderUI({
        code <- style_code(ggplot_rv$code)
        expr <- output_filter$expr()
        if (!is.null(expr) & !isTRUE(input$disable_filters)) {
          code_dplyr <- deparse2(output_filter$code())
          nm_dat <- data_name()
          code <- gsub(x = code, replacement = " ggplot()", pattern = sprintf("ggplot(%s)", nm_dat), fixed = TRUE)
          code <- paste(code_dplyr, code, sep = " %>%\n")
        }
        htmltools::tagList(
          rCodeContainer(id = ns("codeggplot"), code)
        )
      })



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

      observeEvent({
        all_inputs <- reactiveValuesToList(input)
        all_inputs[grep(pattern = "filter-data", x = names(all_inputs), invert = TRUE)]
      }, {
        all_inputs <- reactiveValuesToList(input)
        # remove inputs from filterDataServer module with ID "filter-data"
        inputs <- all_inputs[grep(pattern = "filter-data", x = names(all_inputs), invert = TRUE)]
        inputs <- inputs[grep(pattern = "^labs_", x = names(inputs), invert = TRUE)]
        inputs <- inputs[grep(pattern = "^export_", x = names(inputs), invert = TRUE)]
        inputs <- inputs[order(names(inputs))]

        outputs$inputs <- modifyList(outputs$inputs, inputs)
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
              axis.text.x = theme_appearance$axis_text_x
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








#' Controls for code and export
#'
#' Display code for reproduce chart and export button
#'
#' @param ns Namespace from module
#'
#' @noRd
#' @importFrom shiny downloadButton uiOutput actionLink
#' @importFrom htmltools tagList tags
#'
controls_code <- function(ns, insert_code = FALSE) {
  tagList(
    tags$button(
      class = "btn btn-link btn-xs pull-right float-end btn-copy-code",
      i18n("Copy to clipboard"),
      `data-clipboard-target` = paste0("#", ns("codeggplot"))
    ), tags$script("$(function() {new ClipboardJS('.btn-copy-code');});"),
    tags$br(),
    tags$b(i18n("Code:")),
    uiOutput(outputId = ns("code")),
    tags$textarea(id = ns("holderCode"), style = "display: none;"),
    if (insert_code) {
      actionLink(
        inputId = ns("insert_code"),
        label = tagList(ph("arrow-circle-left"), i18n("Insert code in script"))
      )
    },
    tags$br()
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


