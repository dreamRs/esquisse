
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
#       class = "btn-esquisse-controls"
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
#' @importFrom shiny icon checkboxInput
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
          controls_labs(ns),
          inputId = ns("controls-labs"),
          class = "esquisse-controls-labs",
          style = "default",
          label = "Labels & Title",
          up = TRUE,
          icon = icon("font"),
          status = "default btn-esquisse-controls"
        )
      },
      if (isTRUE("parameters" %in% controls)) {
        dropdown_(
          controls_params(ns),
          inputId = ns("controls-parameters"),
          class = "esquisse-controls-parameters",
          style = "default",
          label = "Plot options",
          up = TRUE,
          icon = icon("gears"),
          status = "default btn-esquisse-controls"
        )
      },
      if (isTRUE("appearance" %in% controls)) {
        dropdown_(
          controls_appearance(ns),
          inputId = ns("controls-appearance"),
          class = "esquisse-controls-appearance",
          style = "default",
          label = "Appearance",
          up = TRUE,
          icon = icon("palette"),
          status = "default btn-esquisse-controls"
        )
      },
      if (isTRUE("filters" %in% controls)) {
        dropdown_(
          filter_data_ui(id = ns("filter-data")),
          inputId = ns("controls-filters"),
          class = "esquisse-controls-filters",
          style = "default",
          label = "Data",
          up = TRUE,
          icon = icon("filter"),
          status = "default btn-esquisse-controls"
        )
      },
      if (isTRUE("code" %in% controls)) {
        dropdown_(
          controls_code(ns, insert_code = insert_code),
          inputId = ns("controls-code"),
          class = "esquisse-controls-code",
          style = "default",
          label = "Code",
          up = TRUE,
          right = TRUE,
          icon = icon("code"),
          status = "default btn-esquisse-controls"
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
                            use_transY = reactive(FALSE)) {

  callModule(
    id = id, 
    module = function(input, output, session) {
      ns <- session$ns
      
      # Reset labs ----
      observeEvent(data_table(), {
        updateTextInput(session = session, inputId = "labs_title", value = character(0))
        updateTextInput(session = session, inputId = "labs_subtitle", value = character(0))
        updateTextInput(session = session, inputId = "labs_caption", value = character(0))
        updateTextInput(session = session, inputId = "labs_x", value = character(0))
        updateTextInput(session = session, inputId = "labs_y", value = character(0))
        updateTextInput(session = session, inputId = "labs_fill", value = character(0))
        updateTextInput(session = session, inputId = "labs_color", value = character(0))
        updateTextInput(session = session, inputId = "labs_size", value = character(0))
        updateTextInput(session = session, inputId = "labs_shape", value = character(0))
      })

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
      
      
      
      # Controls ----
      
      observeEvent(aesthetics(), {
        aesthetics <- names(aesthetics())
        toggleDisplay(id = ns("controls-labs-fill"), display = "fill" %in% aesthetics)
        toggleDisplay(id = ns("controls-labs-color"), display = "color" %in% aesthetics)
        toggleDisplay(id = ns("controls-labs-size"), display = "size" %in% aesthetics)
        toggleDisplay(id = ns("controls-labs-shape"), display = "shape" %in% aesthetics)
        toggleDisplay(id = ns("controls-ribbon-color"), display = "ymin" %in% aesthetics)
      })
      
      observeEvent(use_facet(), {
        toggleDisplay(id = ns("controls-facet"), display = isTRUE(use_facet()))
      })
      
      observeEvent(use_transX(), {
        toggleDisplay(id = ns("controls-scale-trans-x"), display = isTRUE(use_transX()))
      })
      
      observeEvent(use_transY(), {
        toggleDisplay(id = ns("controls-scale-trans-y"), display = isTRUE(use_transY()))
      })
      
      observeEvent(type$palette, {
        toggleDisplay(id = ns("controls-palette"), display = isTRUE(type$palette))
        toggleDisplay(id = ns("controls-fill-color"), display = !isTRUE(type$palette))
      })
      
      observe({
        aesthetics <- names(aesthetics())
        toggleDisplay(id = ns("controls-shape"), display = type$x %in% "point" & !"shape" %in% aesthetics)
      })
      
      observeEvent(type$x, {
        toggleDisplay(id = ns("controls-position"), display = type$x %in% c("bar", "line", "area"))
        toggleDisplay(id = ns("controls-histogram"), display = type$x %in% "histogram")
        toggleDisplay(id = ns("controls-density"), display = type$x %in% c("density", "violin"))
        toggleDisplay(id = ns("controls-scatter"), display = type$x %in% "point")
        toggleDisplay(id = ns("controls-size"), display = type$x %in% c("point", "line", "step", "sf"))
        toggleDisplay(id = ns("controls-violin"), display = type$x %in% "violin")
        
        if (type$x %in% c("point")) {
          updateSliderInput(session = session, inputId = "size", value = 1.5)
        } else if (type$x %in% c("line", "step")) {
          updateSliderInput(session = session, inputId = "size", value = 0.5)
        }
      })
      
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
        name = data_name
      )
      
      outputs <- reactiveValues(
        inputs = NULL,
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
        outputs$inputs <- inputs
      })
      
      # labs input
      labs_r <- debounce(reactive({
        asth <- names(aesthetics())
        labs_fill <- `if`(isTRUE("fill" %in% asth), input$labs_fill, "")
        labs_color <- `if`(isTRUE("color" %in% asth), input$labs_color, "")
        labs_size <- `if`(isTRUE("size" %in% asth), input$labs_size, "")
        labs_shape <- `if`(isTRUE("shape" %in% asth), input$labs_shape, "")
        list(
          x = input$labs_x %empty% NULL,
          y = input$labs_y %empty% NULL,
          title = input$labs_title %empty% NULL,
          subtitle = input$labs_subtitle %empty% NULL,
          caption = input$labs_caption %empty% NULL,
          fill = labs_fill %empty% NULL,
          color = labs_color %empty% NULL,
          size = labs_size %empty% NULL,
          shape = labs_shape %empty% NULL
        )
      }), millis = 1000)
      observe({
        outputs$labs <- labs_r()
      })
      
      # Colors input
      colors_r <- palette_server("colors", reactive({
        data_ <- data_table()
        aesthetics_ <- aesthetics()
        if ("fill" %in% names(aesthetics_)) {
          return(data_[[aesthetics_$fill]])
        }
        if ("color" %in% names(aesthetics_)) {
          return(data_[[aesthetics_$color]])
        }
        return(character(0))
      }))
      colors_r_d <- debounce(colors_r, millis = 1000)
      observe({
        outputs$colors <- colors_r_d()
      })
      
      
      # limits input
      observe({
        outputs$limits <- list(
          x = use_transX() & !anyNA(input$xlim),
          xlim = input$xlim,
          y = use_transY() & !anyNA(input$ylim),
          ylim = input$ylim
        )
      })
      
      
      # facet input
      observe({
        outputs$facet <- list(
          scales = if (identical(input$facet_scales, "fixed")) NULL else input$facet_scales,
          ncol = if (is.null(input$facet_ncol) || input$facet_ncol == 0) {
            NULL
          } else {
            input$facet_ncol
          },
          nrow = if (is.null(input$facet_ncol) || input$facet_nrow == 0) {
            NULL
          } else {
            input$facet_nrow
          }
        )
      })
      
      # theme input
      observe({
        inputs <- reactiveValuesToList(input)
        title <- get_labs_options(inputs, "title")
        subtitle <- get_labs_options(inputs, "subtitle")
        caption <- get_labs_options(inputs, "caption")
        x <- get_labs_options(inputs, "x")
        y <- get_labs_options(inputs, "y")
        outputs$theme <- list(
          theme = input$theme,
          args = dropNulls(
            list(
              legend.position = if (identical(input$legend_position, "right")) NULL else input$legend_position,
              plot.title = title,
              plot.subtitle = subtitle,
              plot.caption = caption,
              axis.title.y = y,
              axis.title.x = x
            )
          )
        )
      })
      
      # coord input
      observe({
        outputs$coord <- if (isTRUE(input$flip)) "flip" else NULL
      })
      
      # smooth input
      observe({
        outputs$smooth <- list(
          add = input$smooth_add,
          args = list(
            span = input$smooth_span
          )
        )
      })
      
      # transX input
      observe({
        outputs$transX <- list(
          use = use_transX() & !identical(input$transX, "identity"),
          args = list(
            trans = input$transX
          )
        )
      })
      
      # transY input
      observe({
        outputs$transY <- list(
          use = use_transY() & !identical(input$transY, "identity"),
          args = list(
            trans = input$transY
          )
        )
      })
      
      observeEvent(output_filter$filtered(), {
        if (!isTRUE(input$disable_filters)) {
          outputs$data <- output_filter$filtered()
          outputs$code <- output_filter$code()
        }
      })
      
      return(outputs)
    }
  )
}



#' Controls for labs
#'
#' Set title, subtitle, caption, xlab, ylab
#'
#' @param ns Namespace from module
#'
#' @noRd
#' @importFrom htmltools tagList tags
#'
controls_labs <- function(ns) {
  tags$div(
    class = "form-group",
    labs_options_input(
      inputId = ns("labs_title"), 
      placeholder = "Title", 
      label = "Title:",
      defaults = get_labs_defaults("title")
    ),
    labs_options_input(
      inputId = ns("labs_subtitle"), 
      placeholder = "Subtitle",
      label = "Subtitle:",
      defaults = get_labs_defaults("subtitle")
    ),
    labs_options_input(
      inputId = ns("labs_caption"), 
      placeholder = "Caption",
      label = "Caption:",
      defaults = get_labs_defaults("caption")
    ),
    labs_options_input(
      inputId = ns("labs_x"), 
      placeholder = "X label", 
      label = "X label:",
      defaults = get_labs_defaults("x")
    ),
    labs_options_input(
      inputId = ns("labs_y"),
      placeholder = "Y label", 
      label = "Y label:",
      defaults = get_labs_defaults("y")
    ),
    tags$div(
      id = ns("controls-labs-fill"), 
      style = "display: none;",
      textInput(inputId = ns("labs_fill"), placeholder = "Fill label", label = "Fill label:")
    ),
    tags$div(
      id = ns("controls-labs-color"), 
      style = "display: none;",
      textInput(inputId = ns("labs_color"), placeholder = "Color label", label = "Color label:")
    ),
    tags$div(
      id = ns("controls-labs-size"), 
      style = "display: none;",
      textInput(inputId = ns("labs_size"), placeholder = "Size label", label = "Size label:")
    ),
    tags$div(
      id = ns("controls-labs-shape"), 
      style = "display: none;",
      textInput(inputId = ns("labs_shape"), placeholder = "Shape label", label = "Shape label:")
    )
  )
}

#' @importFrom htmltools tags
#' @importFrom shinyWidgets dropMenu prettyRadioButtons
#' @importFrom shiny actionButton icon numericInput
labs_options_input <- function(inputId, label, placeholder, defaults = list()) {
  tags$div(
    class = "esquisse-labs-options",
    tags$div(
      class = "form-group shiny-input-container",
      style = "width: 100%;",
      tags$label(
        class = "control-label",
        id = paste0(inputId, "-label"), 
        `for` = inputId,
        label
      ),
      tags$input(
        id = inputId,
        type = "text", 
        class = "form-control",
        value = "",
        placeholder = placeholder,
        style = "border-radius: 4px 0 0 4px;"
      )
    ),
    dropMenu(
      actionButton(
        inputId = paste0(inputId, "_options"), 
        label = NULL, 
        icon = icon("plus"),
        style = "margin-top: 25px; border-radius: 0 4px 4px 0; width: 100%;"
      ),
      style = "width: 320px;",
      prettyRadioButtons(
        inputId = paste0(inputId, "_face"),
        label = "Font face:",
        choiceNames = c("Plain", "Italic", "Bold", "Bold/Italic"),
        choiceValues = c("plain", "italic", "bold", "bold.italic"),
        selected = defaults$face,
        status = "primary",
        inline = TRUE
      ),
      numericInput(
        inputId = paste0(inputId, "_size"),
        label = "Font size:",
        value = defaults$size
      ),
      prettyRadioButtons(
        inputId = paste0(inputId, "_align"),
        label = "Align:",
        choiceNames = c("Left", "Center", "Right"),
        choiceValues = c("left", "center", "right"),
        inline = TRUE,
        status = "primary",
        selected = switch(
          as.character(defaults$hjust),
          "0" = "left",
          "0.5" = "center",
          "1" = "right"
        )
      ),
      placement = "right"
    )
  )
}

get_labs_defaults <- function(name = c("title", "subtitle", "caption", "x", "y")) {
  name <- match.arg(name)
  defaults_labs <- list(
    title = list(size = 13L, face = "plain", hjust = 0), # theme_get()$plot.title
    subtitle = list(size = 11L, face = "plain", hjust = 0), # theme_get()$plot.subtitle
    caption = list(size = 9L, face = "plain", hjust = 1), # theme_get()$plot.caption
    x = list(size = 11L, face = "plain", hjust = 0.5),
    y = list(size = 11L, face = "plain", hjust = 0.5)
  )
  defaults_labs[[name]]
}

get_labs_options <- function(inputs, name = c("title", "subtitle", "caption", "x", "y")) {
  name <- match.arg(name)
  defaults <- get_labs_defaults(name)
  inputs <- inputs[paste0("labs_", name, c("_size", "_face", "_align"))]
  names(inputs) <- c("size", "face", "align")
  if (length(inputs$align) < 1) {
    inputs$hjust <- defaults$hjust
  } else {
    inputs$hjust <- switch(
      inputs$align,
      "left" = 0,
      "center" = 0.5,
      "right" = 1
    )
  }
  inputs$align <- NULL
  for (nm in names(defaults)) {
    if (identical(inputs[[nm]], defaults[[nm]])) {
      inputs[[nm]] <- NULL
    }
  }
  inputs <- dropNulls(inputs)
  if (length(inputs) > 0) {
    call2("element_text", !!!inputs)
  } else {
    NULL
  }
}



#' Controls for appearance
#'
#' Set color, palette, theme, legend position
#'
#' @param ns Namespace from module
#'
#' @noRd
#' 
#' @importFrom utils head
#' @importFrom shiny icon
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets pickerInput radioGroupButtons colorPickr
controls_appearance <- function(ns) {

  themes <- get_themes()
  cols <- get_colors()
  pals <- get_palettes()
  
  shape_names <- c(
    "circle", paste("circle", c("open", "filled", "cross", "plus", "small")), "bullet",
    "square", paste("square", c("open", "filled", "cross", "plus", "triangle")),
    "diamond", paste("diamond", c("open", "filled", "plus")),
    "triangle", paste("triangle", c("open", "filled", "square")),
    paste("triangle down", c("open", "filled")),
    "plus", "cross", "asterisk"
  )

  tagList(
    tags$div(
      id = ns("controls-fill-color"), style = "display: block;",
      shinyWidgets::colorPickr(
        inputId = ns("fill_color"),
        label = "Color:",
        theme = "monolith",
        update = "changestop",
        inline = TRUE,
        swatches = head(unlist(cols, use.names = FALSE), 9),
        preview = FALSE,
        interaction = list(
          hex = FALSE,
          rgba = FALSE,
          input = TRUE,
          save = FALSE,
          clear = FALSE
        ),
        width = "100%"
      )
    ),
    tags$div(
      id = ns("controls-palette"), style = "display: none;",
      palette_ui(ns("colors"))
    ),
    tags$div(
      id = ns("controls-ribbon-color"), style = "display: none;",
      colorPickr(
        inputId = ns("color_ribbon"),
        selected = "#A4A4A4",
        label = "Ribbon color:",
        theme = "nano",
        useAsButton = TRUE,
        update = "save",
        interaction = list(
          hex = FALSE,
          rgba = FALSE,
          input = TRUE,
          save = TRUE,
          clear = FALSE
        )
      )
    ),
    tags$div(
      id = ns("controls-shape"), style = "display: none;",
      pickerInput(
        inputId = ns("shape"),
        label = "Point symbol:",
        choices = shape_names,
        selected = "circle",
        options = list(size = 10, container = "body"),
        width = "100%"
      )
    ),
    pickerInput(
      inputId = ns("theme"),
      label = "Theme:",
      choices = themes,
      selected = getOption("esquisse.default.theme", default = "theme_minimal"),
      options = list(size = 10, container = "body"),
      width = "100%"
    ),
    tags$script(
      paste0("$('#", ns("theme"), "').addClass('dropup');")
    ),
    radioGroupButtons(
      inputId = ns("legend_position"),
      label = "Legend position:",
      choiceNames = list(
        icon("arrow-left"), icon("arrow-up"),
        icon("arrow-down"), icon("arrow-right"), icon("close")
      ),
      choiceValues = c("left", "top", "bottom", "right", "none"),
      selected = "right",
      justified = TRUE,
      size = "sm"
    )
  )
}



#' Controls for parameters
#'
#' Set bins for histogram, position for barchart, flip coordinates
#'
#' @param ns Namespace from module
#'
#' @noRd
#' @importFrom shiny sliderInput conditionalPanel selectInput numericInput
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets prettyRadioButtons numericRangeInput prettyToggle
#'
controls_params <- function(ns) {

  scales_trans <- c(
    "asn", "atanh", "boxcox", "exp", "identity",
    "log", "log10", "log1p", "log2", "logit",
    "probability", "probit", "reciprocal",
    "reverse", "sqrt"
  )

  tagList(
    tags$div(
      id = ns("controls-scatter"),
      style = "display: none; padding-top: 10px;",
      tags$label(
        class = "control-label",
        `for` = ns("smooth_add"),
        "Add a smooth line:"
      ),
      prettyToggle(
        inputId = ns("smooth_add"),
        label_on = "Yes",
        icon_on = icon("check"),
        status_on = "success",
        status_off = "danger",
        label_off = "No",
        icon_off = icon("remove"),
        inline = TRUE
      ),
      conditionalPanel(
        condition = paste0("input.smooth_add==true"),
        ns = ns,
        sliderInput(
          inputId = ns("smooth_span"),
          label = "Smooth line span:",
          min = 0.1, 
          max = 1,
          value = 0.75, 
          step = 0.01,
          width = "100%"
        )
      ),
    ),
    tags$div(
      id = ns("controls-size"), style = "display: none;",
      sliderInput(
        inputId = ns("size"),
        label = "Size for points/lines:",
        min = 0.5, 
        max = 4,
        value = 1.2,
        width = "100%"
      )
    ),
    tags$div(
      id = ns("controls-facet"), style = "display: none;",
      prettyRadioButtons(
        inputId = ns("facet_scales"),
        label = "Facet scales:",
        inline = TRUE,
        status = "primary",
        choices = c("fixed", "free", "free_x", "free_y"),
        outline = TRUE,
        icon = icon("check")
      ),
      sliderInput(
        inputId = ns("facet_ncol"),
        label = "Facet ncol:",
        min = 0, max = 10,
        value = 0, step = 1
      ),
      sliderInput(
        inputId = ns("facet_nrow"),
        label = "Facet nrow:",
        min = 0, max = 10,
        value = 0, step = 1
      )
    ),
    tags$div(
      id = ns("controls-histogram"), style = "display: none;",
      sliderInput(
        inputId = ns("bins"),
        label = "Numbers of bins:",
        min = 10, max = 100,
        value = 30,
        width = "100%"
      )
    ),
    tags$div(
      id = ns("controls-violin"), style = "display: none;",
      prettyRadioButtons(
        inputId = ns("scale"),
        label = "Scale:",
        inline = TRUE,
        status = "primary",
        choices = c("area", "count", "width"),
        outline = TRUE,
        icon = icon("check")
      )
    ),
    tags$div(
      id = ns("controls-scale-trans-x"), style = "display: none;",
      numericRangeInput(
        inputId = ns("xlim"),
        label = "X-Axis limits (empty for none):",
        value = c(NA, NA)
      ),
      selectInput(
        inputId = ns("transX"),
        label = "X-Axis transform:",
        selected = "identity",
        choices = scales_trans,
        width = "100%"
      )
    ),
    tags$div(
      id = ns("controls-scale-trans-y"), style = "display: none;",
      numericRangeInput(
        inputId = ns("ylim"),
        label = "Y-Axis limits (empty for none):",
        value = c(NA, NA)
      ),
      selectInput(
        inputId = ns("transY"),
        label = "Y-Axis transform:",
        selected = "identity",
        choices = scales_trans,
        width = "100%"
      )
    ),
    tags$div(
      id = ns("controls-density"),
      style = "display: none;",
      sliderInput(
        inputId = ns("adjust"),
        label = "Bandwidth adjustment:",
        min = 0.2,
        max = 6,
        value = 1,
        step = 0.1,
        width = "100%"
      )
    ),
    tags$div(
      id = ns("controls-position"),
      style = "display: none;",
      prettyRadioButtons(
        inputId = ns("position"),
        label = "Position:",
        choices = c("stack", "dodge", "fill"),
        inline = TRUE,
        selected = "stack",
        status = "primary",
        outline = TRUE
      )
    ),
    tags$label(
      class = "control-label",
      `for` = ns("flip"),
      "Flip coordinate:"
    ),
    prettyToggle(
      inputId = ns("flip"),
      label_on = "Yes",
      icon_on = icon("check"),
      status_on = "success",
      status_off = "danger",
      label_off = "No",
      icon_off = icon("remove"),
      inline = TRUE
    )
  )
}


#' Controls for code and export
#'
#' Display code for reproduce chart and export button
#'
#' @param ns Namespace from module
#'
#' @noRd
#' @importFrom shiny icon downloadButton uiOutput actionLink
#' @importFrom htmltools tagList tags
#'
controls_code <- function(ns, insert_code = FALSE) {
  tagList(
    tags$button(
      class = "btn btn-default btn-xs pull-right btn-copy-code",
      "Copy to clipboard", `data-clipboard-target` = paste0("#", ns("codeggplot"))
    ), tags$script("$(function() {new ClipboardJS('.btn-copy-code');});"),
    tags$br(),
    tags$b("Code:"),
    uiOutput(outputId = ns("code")),
    tags$textarea(id = ns("holderCode"), style = "display: none;"),
    if (insert_code) {
      actionLink(
        inputId = ns("insert_code"),
        label = "Insert code in script",
        icon = icon("arrow-circle-left")
      )
    },
    tags$br()
  )
}



# Get list of themes
get_themes <- function() {
  themes <- getOption("esquisse.themes")
  if (is.null(themes))
    themes <- default_themes()
  if (is.function(themes))
    themes <- themes()
  if (!is.list(themes)) {
    stop("Option 'esquisse.themes' must be a list", call. = FALSE)
  }
  themes <- rapply(
    object = themes, 
    f = function(x) {
      if (all(check_theme_exist(x))) {
        x
      } else {
        warning(paste("Theme", x, "not found!"), call. = FALSE)
        NULL
      }
    }, how = "list"
  )
  dropNullsOrEmptyRecursive(themes)
}

# Get list of palettes
get_palettes <- function() {
  pals <- getOption("esquisse.palettes")
  if (is.null(pals))
    pals <- default_pals()
  if (is.function(pals))
    pals <- pals()
  if (!is.list(pals)) {
    stop("Option 'esquisse.palettes' must be a list with at least one slot : 'choices'", call. = FALSE)
  }
  if (is.null(pals$textColor))
    pals$textColor <- "white"
  pals
}

# Get list of colors (spectrum)
get_colors <- function() {
  cols <- getOption("esquisse.colors")
  if (is.null(cols))
    cols <- default_cols()
  if (is.function(cols))
    cols <- cols()
  # if (!is.character(cols)) {
  #   stop("Option 'esquisse.colors' must be a character vector", call. = FALSE)
  # }
  cols
}






select_geom_controls <- function(x, geoms) {
  if (length(x) < 1)
    return("auto")
  if ("bar" %in% geoms & x %in% c("auto", "bar")) {
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
  } else if ("sf" %in% geoms & x %in% c("sf")) {
    "sf"
  } else {
    "auto"
  }
}


