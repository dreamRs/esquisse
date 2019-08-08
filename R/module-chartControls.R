
#' Dropup buttons to hide chart's controls
#'
#' @param id Module id. See \code{\link[shiny]{callModule}}.
#' @param insert_code Logical, Display or not a button to isert the ggplot
#'  code in the current user script (work only in RStudio).
#'
#' @return a \code{\link[shiny]{tagList}} containing UI elements
#' @noRd
#'
#' @importFrom shinyWidgets dropdown
#' @importFrom htmltools tags tagList
#' @importFrom shiny icon
#'
chartControlsUI <- function(id, insert_code = FALSE) {

  # Namespace
  ns <- NS(id)

  # ui
  tags$div(
    class = "btn-group-charter btn-group-justified-charter",
    tags$style(sprintf(
      "#%s .sw-dropdown-in {margin: 8px 0 8px 10px !important; padding: 0 !important;}",
      "sw-content-filterdrop"
    )),
    dropdown(
      controls_labs(ns),
      inputId = "labsdrop",
      style = "default",
      label = "Labels & Title", 
      up = TRUE, 
      icon = icon("font"), 
      status = "default btn-controls"
    ),
    dropdown(
      controls_params(ns), controls_appearance(ns),
      style = "default",
      label = "Plot options",
      up = TRUE, 
      inputId = "paramsdrop",
      icon = icon("gears"), 
      status = "default btn-controls"
    ),
    dropdown(
      tags$div(
        style = "max-height: 400px; overflow-y: scroll; overflow-x: hidden;", #  padding-left: 10px;
        filterDF_UI(id = ns("filter-data"))
      ),
      style = "default", 
      label = "Data", 
      up = TRUE, 
      icon = icon("filter"),
      right = TRUE, 
      inputId = "filterdrop",
      status = "default btn-controls"
    ),
    dropdown(
      controls_code(ns, insert_code = insert_code), 
      style = "default", 
      label = "Export & code", 
      up = TRUE,
      right = TRUE, 
      inputId = "codedrop",
      icon = icon("code"), 
      status = "default btn-controls"
    ),
    tags$script("$('.sw-dropdown').addClass('btn-group-charter');"),
    tags$script(HTML("$('.sw-dropdown > .btn').addClass('btn-charter');")),
    tags$script("$('#sw-content-filterdrop').click(function (e) {e.stopPropagation();});"),
    tags$script("$('#sw-content-filterdrop').css('min-width', '350px');"),
    tags$script("$('#sw-content-codedrop').css('min-width', '350px');"),
    tags$script("$('#sw-content-paramsdrop').css('min-width', '330px');"),
    useShinyUtils()
  )
}




#' Dropup buttons to hide chart's controls
#'
#' @param input,output,session standards \code{shiny} server arguments.
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
#'  downloadHandler renderUI reactive
#' @importFrom rstudioapi insertText getSourceEditorContext
#' @importFrom htmltools tags tagList
#' @importFrom stringi stri_replace_all
#'
chartControlsServer <- function(input, output, session, 
                                type, data_table, data_name,
                                ggplot_rv, 
                                aesthetics = reactive(NULL),
                                use_facet = shiny::reactive(FALSE), 
                                use_transX = shiny::reactive(FALSE), 
                                use_transY = shiny::reactive(FALSE)) {

  ns <- session$ns
  
  # Export ----
  
  output$export_png <- downloadHandler(
    filename = function() {
      paste0("esquisse_", format(Sys.time(), format = "%Y%m%dT%H%M%S"), ".png")
    },
    content = function(file) {
      pngg <- try(ggsave(filename = file, plot = ggplot_rv$ggobj$plot, width = 12, height = 8, dpi = "retina"))
      if ("try-error" %in% class(pngg)) {
        shiny::showNotification(ui = "Export to PNG failed...", type = "error")
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
        ppt <- try(rvg::ph_with_vg(ppt, ggobj = gg, type = "body"), silent = TRUE)
        if ("try-error" %in% class(ppt)) {
          shiny::showNotification(ui = "Export to PowerPoint failed...", type = "error")
        } else {
          tmp <- tempfile(pattern = "esquisse", fileext = ".pptx")
          print(ppt, target = tmp)
          file.copy(from = tmp, to = file)
        }
      } else {
        warn <- "Packages 'officer' and 'rvg' are required to use this functionality."
        warning(warn, call. = FALSE)
        shiny::showNotification(ui = warn, type = "warning")
      }
    }
  )
  
  
  
  # Code ----
  observeEvent(input$insert_code, {
    context <- rstudioapi::getSourceEditorContext()
    code <- ggplot_rv$code
    code <- stri_replace_all(str = code, replacement = "+\n", fixed = "+")
    if (input$insert_code == 1) {
      code <- paste("library(ggplot2)", code, sep = "\n\n")
    }
    if (!is.null(output_filter$code$expr)) {
      code_dplyr <- deparse(output_filter$code$dplyr, width.cutoff = 80L)
      code_dplyr <- paste(code_dplyr, collapse = "\n")
      nm_dat <- data_name()
      code_dplyr <- paste(nm_dat, code_dplyr, sep = " <- ")
      code_dplyr <- stri_replace_all(str = code_dplyr, replacement = "%>%\n", fixed = "%>%")
      code <- paste(code_dplyr, code, sep = "\n\n")
    }
    rstudioapi::insertText(text = paste0("\n", code), id = context$id)
  })
  
  output$code <- renderUI({
    code <- ggplot_rv$code
    code <- stri_replace_all(str = code, replacement = "+\n", fixed = "+")
    if (!is.null(output_filter$code$expr)) {
      code_dplyr <- deparse(output_filter$code$dplyr, width.cutoff = 80L)
      code_dplyr <- paste(code_dplyr, collapse = "\n")
      nm_dat <- data_name()
      code_dplyr <- paste(nm_dat, code_dplyr, sep = " <- ")
      code_dplyr <- stri_replace_all(str = code_dplyr, replacement = "%>%\n", fixed = "%>%")
      code <- paste(code_dplyr, code, sep = "\n\n")
    }
    htmltools::tagList(
      rCodeContainer(id = ns("codeggplot"), code)
    )
  })
  
  
  
  # Controls ----
  
  observeEvent(aesthetics(), {
    aesthetics <- aesthetics()
    if ("fill" %in% aesthetics) {
      toggleDisplay(id = ns("controls-labs-fill"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-labs-fill"), display = "none")
    }
    if ("color" %in% aesthetics) {
      toggleDisplay(id = ns("controls-labs-color"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-labs-color"), display = "none")
    }
    if ("size" %in% aesthetics) {
      toggleDisplay(id = ns("controls-labs-size"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-labs-size"), display = "none")
    }
  })
  
  observeEvent(use_facet(), {
    if (isTRUE(use_facet())) {
      toggleDisplay(id = ns("controls-facet"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-facet"), display = "none")
    }
  })
  
  observeEvent(use_transX(), {
    if (isTRUE(use_transX())) {
      toggleDisplay(id = ns("controls-scale-trans-x"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-scale-trans-x"), display = "none")
    }
  })
  
  observeEvent(use_transY(), {
    if (isTRUE(use_transY())) {
      toggleDisplay(id = ns("controls-scale-trans-y"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-scale-trans-y"), display = "none")
    }
  })

  observeEvent(type$palette, {
    if (isTRUE(type$palette)) {
      toggleDisplay(id = ns("controls-palette"), display = "block")
      toggleDisplay(id = ns("controls-spectrum"), display = "none")
    } else {
      toggleDisplay(id = ns("controls-palette"), display = "none")
      toggleDisplay(id = ns("controls-spectrum"), display = "block")
    }
  })
  
  observeEvent(type$x, {
    if (type$x %in% c("bar", "line", "area")) {
      toggleDisplay(id = ns("controls-position"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-position"), display = "none")
    }
    if (type$x %in% "bar") {
      toggleDisplay(id = ns("controls-flip"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-flip"), display = "none")
    }
    if (type$x %in% "histogram") {
      toggleDisplay(id = ns("controls-histogram"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-histogram"), display = "none")
    }
    if (type$x %in% c("density", "violin")) {
      toggleDisplay(id = ns("controls-density"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-density"), display = "none")
    }
    if (type$x %in% "point") {
      toggleDisplay(id = ns("controls-scatter"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-scatter"), display = "none")
    }
    if (type$x %in% c("point", "line")) {
      toggleDisplay(id = ns("controls-size"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-size"), display = "none")
    }
    if (type$x %in% "violin") {
      toggleDisplay(id = ns("controls-violin"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-violin"), display = "none")
    }
  })
  
  output_filter <- callModule(
    module = filterDF, 
    id = "filter-data", 
    data_table = data_table, 
    data_name = data_name
  )

  outin <- reactiveValues(inputs = NULL, export_ppt = NULL, export_png = NULL)

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
    outin$inputs <- inputs
  })
  
  # labs input
  observe({
    asth <- aesthetics()
    labs_fill <- ifelse("fill" %in% asth, input$labs_fill, "")
    labs_color <- ifelse("color" %in% asth, input$labs_color, "")
    labs_size <- ifelse("size" %in% asth, input$labs_size, "")
    outin$labs <- list(
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
  
  # facet input
  observe({
    outin$facet <- list(
      scales = if (identical(input$facet_scales, "fixed")) NULL else input$facet_scales
    )
  })
  
  # theme input
  observe({
    outin$theme <- list(
      theme = input$theme,
      args = list(
        legend.position = if (identical(input$legend_position, "right")) NULL else input$legend_position
      )
    )
  })
  
  # coord input
  observe({
    outin$coord <- if (input$flip) "flip" else NULL
  })
  
  # smooth input
  observe({
    outin$smooth <- list(
      add = input$smooth_add,
      args = list(
        span = input$smooth_span
      )
    )
  })
  
  # transX input
  observe({
    outin$transX <- list(
      use = use_transX() & !identical(input$transX, "identity"),
      args = list(
        trans = input$transX
      )
    )
  })
  
  # transY input
  observe({
    outin$transY <- list(
      use = use_transY() & !identical(input$transY, "identity"),
      args = list(
        trans = input$transY
      )
    )
  })
  
  observeEvent(output_filter$data_filtered(), {
    outin$data <- output_filter$data_filtered()
    outin$code <- output_filter$code
  })

  return(outin)
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
    textInput(inputId = ns("labs_title"), placeholder = "Title", label = NULL),
    textInput(inputId = ns("labs_subtitle"), placeholder = "Subtitle", label = NULL),
    textInput(inputId = ns("labs_caption"), placeholder = "Caption", label = NULL),
    textInput(inputId = ns("labs_x"), placeholder = "X label", label = NULL),
    textInput(inputId = ns("labs_y"), placeholder = "Y label", label = NULL),
    tags$div(
      id = ns("controls-labs-fill"), style = "display: none;",
      textInput(inputId = ns("labs_fill"), placeholder = "Fill label", label = NULL)
    ),
    tags$div(
      id = ns("controls-labs-color"), style = "display: none;",
      textInput(inputId = ns("labs_color"), placeholder = "Color label", label = NULL)
    ),
    tags$div(
      id = ns("controls-labs-size"), style = "display: none;",
      textInput(inputId = ns("labs_size"), placeholder = "Size label", label = NULL)
    )
  )
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
#' @importFrom shinyWidgets colorSelectorInput pickerInput radioGroupButtons spectrumInput
#' @importFrom ggplot2 theme_bw theme_classic theme_dark theme_gray theme_grey 
#'  theme_light theme_linedraw theme_minimal theme_void
#' @importFrom ggthemes theme_base theme_calc theme_economist theme_economist_white 
#'  theme_excel theme_few theme_fivethirtyeight theme_foundation theme_gdocs theme_hc 
#'  theme_igray theme_map theme_pander theme_par theme_solarized theme_solarized_2 
#'  theme_solid theme_stata theme_tufte theme_wsj
#' @importFrom hrbrthemes theme_ft_rc theme_ipsum theme_ipsum_ps theme_ipsum_rc
#'  theme_ipsum_tw theme_modern_rc
controls_appearance <- function(ns) {

  themes <- list(
    ggplot2 = list(
      "bw", "classic", "dark", "gray",
      "light", "linedraw", "minimal",
      "void"
    ),
    hrbrthemes = c(
      "ft_rc", "ipsum", "ipsum_ps", "ipsum_rc", "ipsum_tw", "modern_rc"
    ),
    ggthemes = list(
      "base", "calc", "economist", "economist_white",
      "excel", "few", "fivethirtyeight", "foundation",
      "gdocs", "hc", "igray", "map", "pander",
      "par", "solarized", "solarized_2", "solid",
      "stata", "tufte", "wsj"
    )
  )

  cols <- colors_palettes()

  tagList(
    tags$div(
      id = ns("controls-spectrum"), style = "display: block;",
      spectrumInput(
        inputId = ns("fill_color"),
        label = "Choose a color:",
        choices = c(list(c("#0C4C8A", "#EF562D")), unname(cols$choices_colors)), 
        width = "100%"
      )
    ),
    tags$div(
      id = ns("controls-palette"), style = "display: none;",
      tags$style(".bootstrap-select .dropdown-menu li a span.text {width: 100%;}"),
      pickerInput(
        inputId = ns("palette"),
        label = "Choose a palette:",
        choices = cols$colors_pal,
        selected = "ggplot2", 
        width = "100%",
        choicesOpt = list(
          content = sprintf(
            "<div style='width:100%%;border-radius:4px; padding: 2px;background:%s;color:%s'>%s</div>",
            unname(cols$background_pals), cols$colortext_pals, names(cols$background_pals)
          )
        )
      )
    ),
    pickerInput(
      inputId = ns("theme"),
      label = "Theme:",
      choices = themes,
      selected = "minimal",
      options = list(size = 10),
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
#' @importFrom shiny sliderInput conditionalPanel selectInput
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets materialSwitch prettyRadioButtons
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
      id = ns("controls-scatter"), style = "display: none; padding-top: 10px;",
      conditionalPanel(
        condition = paste0("input['",  ns("smooth_add"), "']==true"),
        sliderInput(
          inputId = ns("smooth_span"), 
          label = "Span:", 
          min = 0.1, max = 1, 
          value = 0.75, step = 0.01
        )
      ),
      materialSwitch(
        inputId = ns("smooth_add"), 
        label = "Smooth line:",
        right = TRUE, 
        status = "primary"
      )
    ),
    tags$div(
      id = ns("controls-size"), style = "display: none;",
      sliderInput(
        inputId = ns("size"), 
        label = "Size:",
        min = 0.5, max = 3,
        value = 1
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
      )
    ),
    tags$div(
      id = ns("controls-histogram"), style = "display: none;",
      sliderInput(
        inputId = ns("bins"), 
        label = "Numbers of bins:", 
        min = 10, max = 100,
        value = 30
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
      selectInput(
        inputId = ns("transX"), 
        label = "X-Axis transform:",
        selected = "identity", 
        choices = scales_trans
      )
    ),
    tags$div(
      id = ns("controls-scale-trans-y"), style = "display: none;",
      selectInput(
        inputId = ns("transY"), 
        label = "Y-Axis transform:",
        selected = "identity", 
        choices = scales_trans
      )
    ),
    tags$div(
      id = ns("controls-density"), style = "display: none;",
      sliderInput(
        inputId = ns("adjust"), 
        label = "Bandwidth adjustment:", 
        min = 0.2, max = 6, 
        value = 1, step = 0.1
      )
    ),
    tags$div(
      id = ns("controls-position"), style = "display: none;",
      prettyRadioButtons(
        inputId = ns("position"), label = "Position:",
        choices = c("stack", "dodge", "fill"), inline = TRUE,
        selected = "stack", status = "primary",
        outline = TRUE, icon = icon("check")
      )
    ),
    tags$div(
      id = ns("controls-flip"), style = "display: none;",
      materialSwitch(
        inputId = ns("flip"), 
        label = "Flip coordinates:", 
        value = FALSE, 
        status = "primary"
      )
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








#' Colors for picker
#'
#' @noRd
#'
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom scales hue_pal viridis_pal
#' @importFrom hrbrthemes ipsum_pal ft_pal
colors_palettes <- function() {
  ### colors
  # For spectrum pre-defined colors
  choices_colors <- list(
    "viridis" = col2Hex(viridis_pal(option = "viridis")(10)),
    "magma" = col2Hex(viridis_pal(option = "magma")(10)),
    "inferno" = col2Hex(viridis_pal(option = "inferno")(10)),
    "plasma" = col2Hex(viridis_pal(option = "plasma")(10)),
    "cividis" = col2Hex(viridis_pal(option = "cividis")(10))
    ,
    "ipsum" = ipsum_pal()(9),
    "ft" = ft_pal()(9)
    ,
    "Blues" = get_brewer_pal(name = "Blues"),
    "Greens" = get_brewer_pal(name = "Greens"),
    "Reds" = get_brewer_pal(name = "Reds"),
    "Oranges" = get_brewer_pal(name = "Oranges"),
    "Purples" = get_brewer_pal(name = "Purples"),
    "Greys" = get_brewer_pal(name = "Greys"),
    "Dark2" = get_brewer_pal(name = "Dark2"),
    "Set1" = get_brewer_pal(name = "Set1"),
    "Paired" = get_brewer_pal(name = "Paired")
  )

  # For palette picker
  colors_pal <- list(
    Diverging = list(
      "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", 
      "RdGy", "RdYlBu", "RdYlGn", "Spectral"
    ), 
    Qualitative = list(
      "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", 
      "Set2", "Set3"
    ),
    Sequential = list(
      "Blues", "BuGn", "BuPu", 
      "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", 
      "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", 
      "YlOrRd"
    )
  )
  background_pals <- sapply(unlist(colors_pal, use.names = FALSE), get_brewer_pal)
  # add ggplot2 hue & viridis
  background_pals <- c(
    list("ggplot2" = scales::hue_pal()(9)),
    list(
      "viridis" = col2Hex(viridis_pal(option = "viridis")(10)),
      "magma" = col2Hex(viridis_pal(option = "magma")(10)),
      "inferno" = col2Hex(viridis_pal(option = "inferno")(10)),
      "plasma" = col2Hex(viridis_pal(option = "plasma")(10)),
      "cividis" = col2Hex(viridis_pal(option = "cividis")(10))
    ),
    list(
      "ipsum" = ipsum_pal()(9),
      "ft" = ft_pal()(9)
    ),
    background_pals
  )
  background_pals <- unlist(lapply(X = background_pals, FUN = linear_gradient))
  colortext_pals <- rep(c("white", "black", "black"), times = sapply(colors_pal, length))
  colortext_pals <- c("white", rep("white", 5), rep("white", 2), colortext_pals) # ggplot2 + viridis + hrbrthemes

  # add ggplot2 hue & viridis
  colors_pal <- c(
    list("Default" = list("ggplot2")),
    list("Viridis" = list("viridis", "magma", "inferno", "plasma", "cividis")),
    list("hrbrthemes" = list("ipsum", "ft")),
    colors_pal
  )

  list(
    colors_pal = colors_pal,
    background_pals = background_pals,
    colortext_pals = colortext_pals,
    choices_colors = choices_colors
  )
}






select_geom_controls <- function(x, geoms) {
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


