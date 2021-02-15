
#' @importFrom shinyWidgets dropdown
#' @importFrom htmltools tagAppendAttributes
dropdown_ <- function(...) {
  tagAppendAttributes(
    dropdown(...),
    class = "btn-group-esquisse"
  )
}


#' Dropup buttons to hide chart's controls
#'
#' @param id Module's ID. See \code{\link[shiny]{callModule}}.
#' @param insert_code Logical, Display or not a button to isert the ggplot
#'  code in the current user script (work only in RStudio).
#'
#' @return a \code{\link[shiny]{tagList}} containing UI elements
#' @noRd
#'
#' @importFrom htmltools tags tagList HTML
#' @importFrom shiny icon checkboxInput
#'
controls_ui <- function(id,
                        controls = c("labs", "parameters", "colors", "filters", "code"),
                        insert_code = FALSE,
                        disable_filters = FALSE) {
  if (!is.null(controls)) {
    controls <- match.arg(
      controls,
      choices = c("labs", "parameters", "colors", "filters", "code"), 
      several.ok = TRUE
    )
  }
  ns <- NS(id)
  tagList(
    tags$div(
      class = "btn-group-esquisse btn-group-justified-esquisse",
      tags$style(sprintf(
        "#%s .sw-dropdown-in {margin: 8px 0 8px 10px !important; padding: 0 !important;}",
        "sw-content-filterdrop"
      )),
      dropdown_(
        controls_labs(ns),
        inputId = "controls-labs",
        style = "default",
        label = "Labels & Title",
        up = TRUE,
        icon = icon("font"),
        status = "default btn-esquisse-controls"
      ),
      dropdown_(
        controls_params(ns),
        style = "default",
        label = "Plot options",
        up = TRUE,
        inputId = "controls-parameters",
        icon = icon("gears"),
        status = "default btn-esquisse-controls"
      ),
      dropdown_(
        controls_appearance(ns),
        style = "default",
        label = "Appearance",
        up = TRUE,
        inputId = "controls-appearance",
        icon = icon("palette"),
        status = "default btn-esquisse-controls"
      ),
      if (!isTRUE(disable_filters)) {
        dropdown_(
          filterDF_UI(id = ns("filter-data")),
          style = "default",
          label = "Data",
          up = TRUE,
          icon = icon("filter"),
          right = TRUE,
          inputId = "controls-filters",
          status = "default btn-esquisse-controls"
        )
      },
      dropdown_(
        controls_code(ns, insert_code = insert_code),
        style = "default",
        label = "Export & code",
        up = TRUE,
        right = TRUE,
        inputId = "controls-code",
        icon = icon("code"),
        status = "default btn-esquisse-controls"
      )
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
#'  downloadHandler renderUI reactive updateTextInput showNotification callModule
#' @importFrom rstudioapi insertText getSourceEditorContext
#' @importFrom htmltools tags tagList
#' @importFrom stringi stri_replace_all
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
      })
      
      
      # Export ----
      
      output$export_png <- downloadHandler(
        filename = function() {
          paste0("esquisse_", format(Sys.time(), format = "%Y%m%dT%H%M%S"), ".png")
        },
        content = function(file) {
          pngg <- try(ggsave(filename = file, plot = ggplot_rv$ggobj$plot, width = 12, height = 8, dpi = "retina"))
          if ("try-error" %in% class(pngg)) {
            shiny::showNotification(ui = "Export to PNG failed...", type = "error", id = paste("esquisse", sample.int(1e6, 1), sep = "-"))
          }
        }
      )
      output$export_ppt <- downloadHandler(
        filename = function() {
          paste0("esquisse_", format(Sys.time(), format = "%Y%m%dT%H%M%S"), ".pptx")
        },
        content = function(file) {
          if (requireNamespace(package = "rvg") & requireNamespace(package = "officer")) {
            gg <- ggplot_rv$ggobj$plot
            ppt <- officer::read_pptx()
            ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
            ppt <- try(officer::ph_with(ppt, rvg::dml(ggobj = gg), location = officer::ph_location_type(type = "body")), silent = TRUE)
            if ("try-error" %in% class(ppt)) {
              shiny::showNotification(ui = "Export to PowerPoint failed...", type = "error", id = paste("esquisse", sample.int(1e6, 1), sep = "-"))
            } else {
              tmp <- tempfile(pattern = "esquisse", fileext = ".pptx")
              print(ppt, target = tmp)
              file.copy(from = tmp, to = file)
            }
          } else {
            warn <- "Packages 'officer' and 'rvg' are required to use this functionality."
            warning(warn, call. = FALSE)
            shiny::showNotification(ui = warn, type = "warning", paste("esquisse", sample.int(1e6, 1), sep = "-"))
          }
        }
      )
      
      
      
      # Code ----
      observeEvent(input$insert_code, {
        context <- rstudioapi::getSourceEditorContext()
        code <- ggplot_rv$code
        code <- stri_replace_all(str = code, replacement = "+\n", fixed = "+")
        if (!is.null(output_filter$code$expr) & !isTRUE(input$disable_filters)) {
          code_dplyr <- deparse(output_filter$code$dplyr, width.cutoff = 80L)
          code_dplyr <- paste(code_dplyr, collapse = "\n")
          nm_dat <- data_name()
          code_dplyr <- stri_replace_all(str = code_dplyr, replacement = "%>%\n", fixed = "%>%")
          code <- stri_replace_all(str = code, replacement = " ggplot()", fixed = sprintf("ggplot(%s)", nm_dat))
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
        code <- ggplot_rv$code
        code <- stri_replace_all(str = code, replacement = "+\n", fixed = "+")
        if (!is.null(output_filter$code$expr) & !isTRUE(input$disable_filters)) {
          code_dplyr <- deparse(output_filter$code$dplyr, width.cutoff = 80L)
          code_dplyr <- paste(code_dplyr, collapse = "\n")
          nm_dat <- data_name()
          code_dplyr <- stri_replace_all(str = code_dplyr, replacement = "%>%\n", fixed = "%>%")
          code <- stri_replace_all(str = code, replacement = " ggplot()", fixed = sprintf("ggplot(%s)", nm_dat))
          code <- paste(code_dplyr, code, sep = " %>%\n")
        }
        htmltools::tagList(
          rCodeContainer(id = ns("codeggplot"), code)
        )
      })
      
      
      
      # Controls ----
      
      observeEvent(aesthetics(), {
        aesthetics <- aesthetics()
        toggleDisplay(id = ns("controls-labs-fill"), display = "fill" %in% aesthetics)
        toggleDisplay(id = ns("controls-labs-color"), display = "color" %in% aesthetics)
        toggleDisplay(id = ns("controls-labs-size"), display = "size" %in% aesthetics)
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
        toggleDisplay(id = ns("controls-spectrum"), display = !isTRUE(type$palette))
      })
      
      observeEvent(type$x, {
        toggleDisplay(id = ns("controls-position"), display = type$x %in% c("bar", "line", "area"))
        toggleDisplay(id = ns("controls-histogram"), display = type$x %in% "histogram")
        toggleDisplay(id = ns("controls-density"), display = type$x %in% c("density", "violin"))
        toggleDisplay(id = ns("controls-scatter"), display = type$x %in% "point")
        toggleDisplay(id = ns("controls-size"), display = type$x %in% c("point", "line"))
        toggleDisplay(id = ns("controls-violin"), display = type$x %in% "violin")
      })
      
      output_filter <- callModule(
        module = filterDF,
        id = "filter-data",
        data_table = data_table,
        data_name = data_name
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
      observe({
        asth <- aesthetics()
        labs_fill <- ifelse("fill" %in% asth, input$labs_fill, "")
        labs_color <- ifelse("color" %in% asth, input$labs_color, "")
        labs_size <- ifelse("size" %in% asth, input$labs_size, "")
        outputs$labs <- list(
          x = input$labs_x %empty% NULL,
          y = input$labs_y %empty% NULL,
          title = input$labs_title %empty% NULL,
          subtitle = input$labs_subtitle %empty% NULL,
          caption = input$labs_caption %empty% NULL,
          fill = labs_fill %empty% NULL,
          color = labs_color %empty% NULL,
          size = labs_size %empty% NULL
        )
      })
      
      #limits input
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
          ncol = if (input$facet_ncol == 0) NULL else input$facet_ncol,
          nrow = if (input$facet_nrow == 0) NULL else input$facet_nrow
        )
      })
      
      # theme input
      observe({
        inputs <- reactiveValuesToList(input)
        title <- get_labs_options(inputs, "title")
        subtitle <- get_labs_options(inputs, "subtitle")
        x <- get_labs_options(inputs, "x")
        y <- get_labs_options(inputs, "y")
        outputs$theme <- list(
          theme = input$theme,
          args = dropNulls(
            list(
              legend.position = if (identical(input$legend_position, "right")) NULL else input$legend_position,
              plot.title = title,
              plot.subtitle = subtitle,
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
      
      observeEvent(output_filter$data_filtered(), {
        if(!isTRUE(input$disable_filters)){
          outputs$data <- output_filter$data_filtered()
          outputs$code <- output_filter$code
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
    textInput(inputId = ns("labs_caption"), placeholder = "Caption", label = "Caption:"),
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
    )
  )
}

#' @importFrom htmltools tags
#' @importFrom shinyWidgets dropMenu prettyCheckbox prettyRadioButtons
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
      prettyCheckbox(
        inputId = paste0(inputId, "_bold"),
        label = "Bold?",
        value = isTRUE("bold" %in% defaults$face),
        status = "primary"
      ),
      numericInput(
        inputId = paste0(inputId, "_size"),
        label = "Font size:",
        value = defaults$size
      ),
      prettyRadioButtons(
        inputId = paste0(inputId, "_align"),
        label = "Align:",
        choices = c("left", "center", "right"),
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

get_labs_defaults <- function(name = c("title", "subtitle", "x", "y")) {
  name <- match.arg(name)
  defaults_labs <- list(
    title = list(size = 13L, face = "plain", hjust = 0),
    subtitle = list(size = 11L, face = "plain", hjust = 0),
    x = list(size = 11L, face = "plain", hjust = 0.5),
    y = list(size = 11L, face = "plain", hjust = 0.5)
  )
  defaults_labs[[name]]
}

get_labs_options <- function(inputs, name = c("title", "subtitle", "x", "y")) {
  name <- match.arg(name)
  defaults <- get_labs_defaults(name)
  inputs <- inputs[paste0("labs_", name, c("_size", "_bold", "_align"))]
  names(inputs) <- c("size", "bold", "align")
  if (isTRUE(inputs$bold)) {
    inputs$face <- "bold"
  } else {
    inputs$face <- "plain"
  }
  inputs$bold <- NULL
  inputs$hjust <- switch(
    inputs$align,
    "left" = 0,
    "center" = 0.5,
    "right" = 1
  )
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
#' @importFrom shiny icon
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets pickerInput radioGroupButtons spectrumInput
controls_appearance <- function(ns) {

  themes <- get_themes()
  cols <- get_colors()
  pals <- get_palettes()

  tagList(

    tags$style(
      ".bootstrap-select .dropdown-menu li a span.text { display: block !important; }"
    ),

    tags$div(
      id = ns("controls-spectrum"), style = "display: block;",
      # colorPickr(
      #   inputId = ns("fill_color"),
      #   label = "Color:",
      #   theme = "monolith",
      #   update = "changestop",
      #   inline = TRUE,
      #   swatches = unlist(cols, use.names = FALSE),
      #   preview = FALSE,
      #   interaction = list(
      #     hex = FALSE,
      #     rgba = FALSE,
      #     input = TRUE,
      #     save = FALSE,
      #     clear = FALSE
      #   ),
      #   width = "100%"
      # )
      spectrumInput(
        inputId = ns("fill_color"),
        label = "Color:",
        choices = unname(cols), 
        flat = TRUE,
        update_on = "dragstop",
        options = list(
          `toggle-palette-only` = FALSE,
          `show-palette-only` = FALSE,
          `show-buttons` = FALSE,
          `hide-after-palette-select` = TRUE
        ),
        width = "100%"
      )
    ),
    tags$div(
      id = ns("controls-palette"), style = "display: none;",
      palettePicker(
        inputId = ns("palette"),
        label = "Choose a palette:",
        choices = pals$choices,
        textColor = pals$textColor,
        pickerOpts = list(container = "body")
      )
    ),
    pickerInput(
      inputId = ns("theme"),
      label = "Theme:",
      choices = themes,
      selected = getOption("esquisse.default.theme"),
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
        condition = paste0("input['",  ns("smooth_add"), "']==true"),
        sliderInput(
          inputId = ns("smooth_span"),
          label = "Smooth line span:",
          min = 0.1, max = 1,
          value = 0.75, step = 0.01,
          width = "100%"
        )
      ),
    ),
    tags$div(
      id = ns("controls-size"), style = "display: none;",
      sliderInput(
        inputId = ns("size"),
        label = "Size for points/lines:",
        min = 0.5, max = 3,
        value = 1,
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
    ), tags$script("new Clipboard('.btn-copy-code');"),
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
    tags$br(),
    tags$b("Export:"),
    tags$br(),
    tags$div(
      class = "btn-group btn-group-justified",
      downloadButton(
        outputId = ns("export_png"),
        label = ".png",
        class = "btn-primary btn-xs"
      ),
      downloadButton(
        outputId = ns("export_ppt"),
        label = ".pptx",
        class = "btn-primary btn-xs"
      )
    )
  )
}



# Get list of themes
get_themes <- function() {
  themes <- getOption("esquisse.themes")
  if (is.function(themes))
    themes <- themes()
  if (!is.list(themes)) {
    stop("Option 'esquisse.themes' must be a list", call. = FALSE)
  }
  themes
}

# Get list of palettes
get_palettes <- function() {
  pals <- getOption("esquisse.palettes")
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
  } else if ("line" %in% geoms & x %in% c("auto", "line")) {
    "line"
  } else if ("area" %in% geoms & x %in% c("auto", "area")) {
    "area"
  } else if ("violin" %in% geoms & x %in% c("violin")) {
    "violin"
  } else {
    "auto"
  }
}


