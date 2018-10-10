
#' Dropup buttons to hide chart's controls
#'
#' @param id Module's id
#'
#' @return a \code{\link[shiny]{tagList}} containing UI elements
#' @noRd
#'
#' @importFrom shinyWidgets dropdown
#' @importFrom htmltools tags tagList
#' @importFrom shiny icon
#'
chartControlsUI <- function(id) {

  # Namespace
  ns <- NS(id)

  # ui
  tags$div(
    class = "btn-group-charter btn-group-justified-charter",
    dropdown(
      controls_labs(ns), inputId = "labsdrop",
      style = "default", label = "Labels & Title", 
      up = TRUE, icon = icon("font"), 
      status = "default btn-controls"
    ),
    dropdown(
      controls_params(ns), controls_appearance(ns),
      style = "default", label = "Plot options",
      up = TRUE, inputId = "paramsdrop",
      icon = icon("gears"), status = "default btn-controls"
    ),
    dropdown(
      tags$div(
        style = "max-height: 400px; overflow-y: scroll;", #  padding-left: 10px;
        filterDataUI(id = ns("filter-data"))
      ),
      style = "default", label = "Data", 
      up = TRUE, icon = icon("filter"),
      right = TRUE, inputId = "filterdrop",
      status = "default btn-controls"
    ),
    dropdown(
      controls_code(ns), width = "330px",
      style = "default", label = "Export & code", 
      up = TRUE, right = TRUE, inputId = "codedrop",
      icon = icon("code"), status = "default btn-controls"
    ),
    tags$script("$('.sw-dropdown').addClass('btn-group-charter');"),
    tags$script(HTML("$('.sw-dropdown > .btn').addClass('btn-charter');")),
    tags$script("$('#sw-content-filterdrop').click(function (e) {e.stopPropagation();});"),
    useShinyUtils()
  )
}




#' Dropup buttons to hide chart's controls
#'
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param type A reactiveValues indicating the type of chart
#'
#' @return A reactiveValues with all input's values
#' @noRd
#'
#' @importFrom shiny observeEvent reactiveValues reactiveValuesToList
#'
chartControlsServer <- function(input, output, session, type, data = NULL) {

  ns <- session$ns

  observeEvent(type$palette, {
    if (isTRUE(type$palette)) {
      toggleDisplay(session = session, id = ns("controls-palette"), display = "block")
      toggleDisplay(session = session, id = ns("controls-spectrum"), display = "none")
    } else {
      toggleDisplay(session = session, id = ns("controls-palette"), display = "none")
      toggleDisplay(session = session, id = ns("controls-spectrum"), display = "block")
    }
  })
  
  observeEvent(type$x, {
    if (type$x %in% c("bar", "line", "area")) {
      toggleDisplay(session = session, id = ns("controls-position"), display = "block")
    } else {
      toggleDisplay(session = session, id = ns("controls-position"), display = "none")
    }
    if (type$x %in% "bar") {
      toggleDisplay(session = session, id = ns("controls-flip"), display = "block")
    } else {
      toggleDisplay(session = session, id = ns("controls-flip"), display = "none")
    }
    if (type$x %in% "histogram") {
      toggleDisplay(session = session, id = ns("controls-histogram"), display = "block")
    } else {
      toggleDisplay(session = session, id = ns("controls-histogram"), display = "none")
    }
    if (type$x %in% c("density", "violin")) {
      toggleDisplay(session = session, id = ns("controls-density"), display = "block")
    } else {
      toggleDisplay(session = session, id = ns("controls-density"), display = "none")
    }
    if (type$x %in% "point") {
      toggleDisplay(session = session, id = ns("controls-scatter"), display = "block")
    } else {
      toggleDisplay(session = session, id = ns("controls-scatter"), display = "none")
    }
    if (type$x %in% c("point", "line")) {
      toggleDisplay(session = session, id = ns("controls-size"), display = "block")
    } else {
      toggleDisplay(session = session, id = ns("controls-size"), display = "none")
    }
    if (type$x %in% "violin") {
      toggleDisplay(session = session, id = ns("controls-violin"), display = "block")
    } else {
      toggleDisplay(session = session, id = ns("controls-violin"), display = "none")
    }
    if (type$x %in% c("point", "line", "density", "violin")) {
      toggleDisplay(session = session, id = ns("controls-xtrans"), display = "block")
    } else {
      toggleDisplay(session = session, id = ns("controls-xtrans"), display = "none")
    }
    if (type$x == "sf") {
      toggleDisplay(session = session, id = ns("controls-ytrans"), display = "none")
    } else {
      toggleDisplay(session = session, id = ns("controls-ytrans"), display = "block")
    }
  })
  
  res_data <- callModule(
    module = filterDataServer, 
    id = "filter-data", data = data, 
    width = "95%"
  )

  outin <- reactiveValues(inputs = NULL, export_ppt = NULL, export_png = NULL)
  
  observeEvent(input$export_ppt, {
    outin$export_ppt <- input$export_ppt
  }, ignoreInit = TRUE)
  observeEvent(input$export_png, {
    outin$export_png <- input$export_png
  }, ignoreInit = TRUE)

  observeEvent({
    all_inputs <- reactiveValuesToList(input)
    all_inputs[grep(pattern = "filter-data", x = names(all_inputs), invert = TRUE)]
  }, {
    all_inputs <- reactiveValuesToList(input)
    # remove inputs from filterDataServer module with ID "filter-data"
    nofilter_inputs <- all_inputs[grep(pattern = "filter-data", x = names(all_inputs), invert = TRUE)]
    nofilter_inputs <- nofilter_inputs[order(names(nofilter_inputs))]
    outin$inputs <- nofilter_inputs
  })
  
  observeEvent(res_data$data, {
    outin$data <- res_data$data
    outin$code <- res_data$code
    outin$index <- res_data$index
  })

  return(outin)
}









# Utility color func ------------------------------------------------------




#' Convert a color in character into hex format
#'
#' @param col name of a color, e.g. 'steelblue'
#'
#' @return a hex code
#' @noRd
#'
#' @importFrom grDevices rgb col2rgb
#'
col2Hex <- function(col) {
  mat <- grDevices::col2rgb(col, alpha = TRUE)
  grDevices::rgb(mat[1, ]/255, mat[2, ]/255, mat[3,]/255)
}


get_brewer_name <- function(name) {
  pals <- RColorBrewer::brewer.pal.info[rownames(RColorBrewer::brewer.pal.info) %in% name, ]
  res <- lapply(
    X = seq_len(nrow(pals)),
    FUN = function(i) {
      brewer.pal(n = pals$maxcolors[i], name = rownames(pals)[i])
    }
  )
  unlist(res)
}


linear_gradient <- function(cols) {
  x <- round(seq(from = 0, to = 100, length.out = length(cols)+1))
  ind <- c(1, rep(seq_along(x)[-c(1, length(x))], each = 2), length(x))
  m <- matrix(data = paste0(x[ind], "%"), ncol = 2, byrow = TRUE)
  res <- lapply(
    X = seq_len(nrow(m)),
    FUN = function(i) {
      paste(paste(cols[i], m[i, 1]), paste(cols[i], m[i, 2]), sep = ", ")
    }
  )
  res <- unlist(res)
  res <- paste(res, collapse = ", ")
  paste0("linear-gradient(to right, ", res, ");")
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
    textInput(inputId = ns("title"), placeholder = "Title", label = NULL),
    textInput(inputId = ns("subtitle"), placeholder = "Subtitle", label = NULL),
    textInput(inputId = ns("caption"), placeholder = "Caption", label = NULL),
    textInput(inputId = ns("x"), placeholder = "X label", label = NULL),
    textInput(inputId = ns("y"), placeholder = "Y label", label = NULL)
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
#'
controls_appearance <- function(ns) {

  if (requireNamespace("ggthemes", quietly = TRUE)) {
    choices_themes <- ggplot_theme(c("ggplot2", "ggthemes"))
  } else {
    choices_themes <- ggplot_theme("ggplot2")
  }

  cols <- colors_palettes()

  tagList(
    tags$div(
      id = ns("controls-spectrum"), style = "display: block;",
      spectrumInput(
        inputId = ns("fill_color"),
        label = "Choose a color:",
        choices = c(list(c("#0C4C8A", "#EF562D")), unname(cols$choices_colors))
      )
    ),
    tags$div(
      id = ns("controls-palette"), style = "display: none;",
      tags$style(".bootstrap-select .dropdown-menu li a span.text {width: 100%;}"),
      pickerInput(
        inputId = ns("palette"), label = "Choose a palette:",
        choices = cols$colors_pal,
        selected = "ggplot2", width = "100%",
        choicesOpt = list(
          content = sprintf(
            "<div style='width:100%%;border-radius:4px; padding: 2px;background:%s;color:%s'>%s</div>",
            unname(cols$background_pals), cols$colortext_pals, names(cols$background_pals)
          )
        )
      )
    ),
    pickerInput(
      inputId = ns("theme"), label = "Theme:",
      choices = choices_themes,
      selected = "ggplot2::theme_minimal",
      options = list(size = 10)
    ),
    tags$script(
      paste0("$('#", ns("theme"), "').addClass('dropup');")
    ),
    radioGroupButtons(
      inputId = ns("legend_position"), label = "Legend position:",
      choiceNames = list(
        icon("arrow-left"), icon("arrow-up"),
        icon("arrow-down"), icon("arrow-right"), icon("close")
      ),
      choiceValues = c("left", "top", "bottom", "right", "none"),
      selected = "right", justified = TRUE, size = "sm"
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
#' @importFrom utils apropos
#'
controls_params <- function(ns) {
  trans <- sub('_trans$', '', c(
    grep('_trans$', getNamespaceExports('scales'), value = TRUE),
    apropos('_trans$')))
  tagList(
    tags$div(
      id = ns("controls-scatter"), style = "display: none; padding-top: 10px;",
      materialSwitch(
        inputId = ns("smooth_add"), 
        label = "Smooth line:",
        right = TRUE, 
        status = "primary"
      ),
      conditionalPanel(
        condition = paste0("input['",  ns("smooth_add"), "']==true"),
        sliderInput(
          inputId = ns("smooth_span"), 
          label = "Span:", 
          min = 0.1, max = 1, 
          value = 0.75, step = 0.01
        )
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
        inputId = ns("scale"), label = "Scale:", inline = TRUE,
        status = "primary", choices = c("area", "count", "width"),
        outline = TRUE, icon = icon("check")
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
    ),
    tags$div(
      id = ns("controls-xtrans"), style = "display:none;",
      selectInput(
        inputId = ns("xtrans"), label = "X-Axis transform:",
        selected = "identity", choices = trans)
    ),
    tags$div(
      id = ns("controls-ytrans"), style = "display:none;",
      selectInput(
        inputId = ns("ytrans"), label = "Y-Axis transform:",
        selected = "identity", choices = trans)
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
#' @importFrom shiny icon
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets actionGroupButtons
#'
controls_code <- function(ns) {
  tagList(
    moduleCodeUI(id = ns("code")),
    tags$br(),
    tags$b("Export:"),
    actionGroupButtons(
      inputIds = c(ns("export_png"), ns("export_ppt")),
      labels = list(
        tags$span(icon("picture-o"), "PNG"),
        tags$span(icon("file-powerpoint-o"), "PowerPoint")
      ), status = "primary", fullwidth = TRUE
    )
  )
}








#' Colors for picker
#'
#' @noRd
#'
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom scales hue_pal
#' @importFrom viridisLite viridis magma inferno plasma cividis
colors_palettes <- function() {
  ### colors
  # For colorSelector
  choices_colors <- list(
    "viridis" = col2Hex(viridis(10)),
    "magma" = col2Hex(magma(10)),
    "inferno" = col2Hex(inferno(10)),
    "plasma" = col2Hex(plasma(10)),
    "cividis" = col2Hex(cividis(10))
    ,
    "Blues" = brewer.pal(n = 9, name = "Blues"),
    "Greens" = brewer.pal(n = 9, name = "Greens"),
    "Reds" = brewer.pal(n = 9, name = "Reds"),
    "Oranges" = brewer.pal(n = 9, name = "Oranges"),
    "Purples" = brewer.pal(n = 9, name = "Purples"),
    "Greys" = brewer.pal(n = 9, name = "Greys"),
    "Dark2" = brewer.pal(n = 8, name = "Dark2"),
    "Set1" = brewer.pal(n = 8, name = "Set1"),
    "Paired" = brewer.pal(n = 10, name = "Paired")
  )

  # For palette picker

  colors_pal <- lapply(
    X = split(
      x = RColorBrewer::brewer.pal.info,
      f = factor(RColorBrewer::brewer.pal.info$category, labels = c("Diverging", "Qualitative", "Sequential"))
    ),
    FUN = function(x) {
      as.list(rownames(x))
    }
  )
  background_pals <- sapply(unlist(colors_pal, use.names = FALSE), get_brewer_name)
  # add ggplot2 hue & viridis
  background_pals <- c(
    list("ggplot2" = scales::hue_pal()(9)),
    list(
      "viridis" = col2Hex(viridis(10)),
      "magma" = col2Hex(magma(10)),
      "inferno" = col2Hex(inferno(10)),
      "plasma" = col2Hex(plasma(10)),
      "cividis" = col2Hex(cividis(10))
    ),
    background_pals
  )
  background_pals <- unlist(lapply(X = background_pals, FUN = linear_gradient))
  colortext_pals <- rep(c("white", "black", "black"), times = sapply(colors_pal, length))
  colortext_pals <- c("white", rep("white", 5), colortext_pals) # ggplot2 + viridis

  # add ggplot2 hue & viridis
  colors_pal <- c(
    list("Default" = list("ggplot2")),
    list("Viridis" = list("viridis", "magma", "inferno", "plasma", "cividis")),
    colors_pal
  )

  list(colors_pal = colors_pal,
       background_pals = background_pals,
       colortext_pals = colortext_pals,
       choices_colors = choices_colors)
}






select_geom_controls <- function(x, types) {
  if ("bar" %in% types & x %in% c("auto", "bar")) {
    "bar"
  } else if ("histogram" %in% types & x %in% c("auto", "histogram")) {
    "histogram"
  } else if ("density" %in% types & x %in% c("auto", "density")) {
    "density"
  } else if ("point" %in% types & x %in% c("auto", "point")) {
    "point"
  } else if ("line" %in% types & x %in% c("auto", "line")) {
    "line"
  } else if ("area" %in% types & x %in% c("auto", "area")) {
    "area"
  } else if ("violin" %in% types & x %in% c("violin")) {
    "violin"
  } else {
    "auto"
  }
}
