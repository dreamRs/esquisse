
#' Dropup buttons to hide chart's controls
#'
#' @param id Module's id
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @noRd
#'
#' @importFrom shinyWidgets dropdown
#' @importFrom htmltools tags tagList
#'
chartControlsUI <- function(id) {

  # Namespace
  ns <- NS(id)

  # ui
  htmltools::tags$div(
    class = "btn-group-charter btn-group-justified-charter",
    shinyWidgets::dropdown(
      controls_labs(ns),
      style = "default", label = "Labels & Title", up = TRUE
    ),
    shinyWidgets::dropdown(
      controls_params(ns), controls_appearance(ns),
      style = "default", label = "Plot options", up = TRUE, width = "270px"
    ),
    shinyWidgets::dropdown(
      "",
      style = "default", label = "Data", up = TRUE, right = TRUE, width = "240px"
    ),
    shinyWidgets::dropdown(
      controls_code(ns),
      style = "default", label = "Export & code", up = TRUE, right = TRUE, width = "350px"
    ),
    htmltools::tags$script("$('.sw-dropdown').addClass('btn-group-charter');"),
    htmltools::tags$script(HTML("$('.sw-dropdown > .btn').addClass('btn-charter');")),
    toggleDisplayUi()
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
chartControlsServer <- function(input, output, session, type) {

  ns <- session$ns

  shiny::observeEvent(type$palette, {
    if (isTRUE(type$palette)) {
      toggleDisplayServer(session = session, id = ns("controls-palette"), display = "block")
      toggleDisplayServer(session = session, id = ns("controls-spectrum"), display = "none")
    } else {
      toggleDisplayServer(session = session, id = ns("controls-palette"), display = "none")
      toggleDisplayServer(session = session, id = ns("controls-spectrum"), display = "block")
    }
  })
  
  shiny::observeEvent(type$x, {
    if (type$x %in% "bar") {
      toggleDisplayServer(session = session, id = ns("controls-categorical"), display = "block")
    } else {
      toggleDisplayServer(session = session, id = ns("controls-categorical"), display = "none")
    }
    if (type$x %in% "histogram") {
      toggleDisplayServer(session = session, id = ns("controls-histogram"), display = "block")
    } else {
      toggleDisplayServer(session = session, id = ns("controls-histogram"), display = "none")
    }
    if (type$x %in% "density") {
      toggleDisplayServer(session = session, id = ns("controls-density"), display = "block")
    } else {
      toggleDisplayServer(session = session, id = ns("controls-density"), display = "none")
    }
    if (type$x %in% "point") {
      toggleDisplayServer(session = session, id = ns("controls-scatter"), display = "block")
    } else {
      toggleDisplayServer(session = session, id = ns("controls-scatter"), display = "none")
    }
    if (type$x %in% c("point", "line")) {
      toggleDisplayServer(session = session, id = ns("controls-size"), display = "block")
    } else {
      toggleDisplayServer(session = session, id = ns("controls-size"), display = "none")
    }
  })

  outin <- shiny::reactiveValues()

  shiny::observeEvent(shiny::reactiveValuesToList(input), {
    all_inputs <- shiny::reactiveValuesToList(input)
    for (i in names(all_inputs)) {
      outin[[i]] <- all_inputs[[i]]
    }
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

#' @importFrom shiny textInput
#' @importFrom htmltools tags
textInputHorizontal <- function(inputId, label) {
  tags$div(
    class="form-group",
    tags$label(
      class="col-sm-4 control-label",
      `for` = inputId, label
    ),
    tags$div(
      class="col-sm-8",
      textInput(inputId = inputId, label = NULL)
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
#'
controls_appearance <- function(ns) {

  if (requireNamespace("ggthemes", quietly = TRUE)) {
    choices_themes <- ggplot_theme(c("ggplot2", "ggthemes"))
  } else {
    choices_themes <- ggplot_theme("ggplot2")
  }

  cols <- colors_palettes()

  htmltools::tagList(
    htmltools::tags$div(
      id = ns("controls-spectrum"), style = "display: block;",
      shinyWidgets::spectrumInput(
        inputId = ns("fill_color"),
        label = "Choose a color :",
        choices = c(list(c("#0C4C8A", "#EF562D")), unname(cols$choices_colors))
      )
    ),
    # shinyWidgets::colorSelectorInput(
    #   inputId = ns("fill_color"),
    #   label = "Choose color :",
    #   choices = c(
    #     " " = list(c("#0C4C8A", "#EF562D")), cols$choices_colors
    #   ),
    #   selected = "#0C4C8A", display_label = TRUE,
    #   mode = "radio"
    # ),
    htmltools::tags$div(
      id = ns("controls-palette"), style = "display: none;",
      shinyWidgets::pickerInput(
        inputId = ns("palette"), label = "Choose a palette :",
        # choices = c(colors_pal, list("viridis" = c("viridis", "magma", "infierno"))),
        choices = c(list("Default" = "ggplot2"), cols$colors_pal),
        selected = "ggplot2", width = "100%",
        choicesOpt = list(
          content = sprintf(
            "<div style='width:100%%;padding:5px;border-radius:4px;background:%s;color:%s'>%s</div>",
            unname(cols$background_pals), cols$colortext_pals, names(cols$background_pals)
          )
        )
      )
    ),
    shinyWidgets::pickerInput(
      inputId = ns("theme"), label = "Theme",
      choices = choices_themes,
      selected = "ggplot2::theme_minimal",
      options = list(size = 10)
    ),
    htmltools::tags$script(
      paste0("$('#", ns("theme"), "').addClass('dropup');")
    ),
    shinyWidgets::radioGroupButtons(
      inputId = ns("legend_position"), label = "Legend position:",
      choiceNames = list(
        shiny::icon("arrow-left"), shiny::icon("arrow-up"),
        shiny::icon("arrow-down"), shiny::icon("arrow-right"), shiny::icon("close")
      ),
      choiceValues = c("left", "top", "bottom", "right", "none"),
      selected = "right", justified = TRUE
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
#' @importFrom shiny sliderInput conditionalPanel
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets awesomeRadio materialSwitch
#'
controls_params <- function(ns) {
  htmltools::tagList(
    htmltools::tags$div(
      id = ns("controls-scatter"), style = "display: none; padding-top: 10px;",
      shinyWidgets::materialSwitch(inputId = ns("smooth_add"), label = "Smooth line", right = TRUE, status = "primary"),
      shiny::conditionalPanel(
        condition = paste0("input['",  ns("smooth_add"), "']==true"),
        shiny::sliderInput(inputId = ns("smooth_span"), label = "Span:", min = 0.1, max = 1, value = 0.75, step = 0.01)
      )
    ),
    htmltools::tags$div(
      id = ns("controls-size"), style = "display: none;",
      shiny::sliderInput(inputId = ns("size"), label = "Size:", min = 0.5, max = 3, value = 1)
    ),
    htmltools::tags$div(
      id = ns("controls-histogram"), style = "display: none;",
      shiny::sliderInput(inputId = ns("bins"), label = "Numbers of bins", min = 10, max = 100, value = 30)
    ),
    htmltools::tags$div(
      id = ns("controls-density"), style = "display: none;",
      shiny::sliderInput(inputId = ns("adjust"), label = "Bandwidth adjustment", min = 0.2, max = 6, value = 1, step = 0.1)
    ),
    htmltools::tags$div(
      id = ns("controls-categorical"), style = "display: none;",
      shinyWidgets::awesomeRadio(
        inputId = ns("position"), label = "Position :",
        choices = c("stack", "dodge", "fill"),
        selected = "stack", inline = TRUE, checkbox = TRUE
      ),
      shinyWidgets::materialSwitch(inputId = ns("flip"), label = "Flip coordinates", value = FALSE, status = "primary")
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
  htmltools::tagList(
    moduleCodeUI(id = "code"),
    htmltools::tags$br(),
    htmltools::tags$b("Export:"),
    shinyWidgets::actionGroupButtons(
      inputIds = c(ns("export_png"), ns("export_ppt")),
      labels = list(
        htmltools::tags$span(shiny::icon("picture-o"), "PNG"),
        htmltools::tags$span(shiny::icon("file-powerpoint-o"), "PowerPoint")
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
colors_palettes <- function() {
  ### colors
  # For colorSelector
  choices_colors <- list(
    "Blues" = RColorBrewer::brewer.pal(n = 9, name = "Blues"),
    "Greens" = RColorBrewer::brewer.pal(n = 9, name = "Greens"),
    "Reds" = RColorBrewer::brewer.pal(n = 9, name = "Reds"),
    "Oranges" = RColorBrewer::brewer.pal(n = 9, name = "Oranges"),
    "Purples" = RColorBrewer::brewer.pal(n = 9, name = "Purples"),
    "Greys" = RColorBrewer::brewer.pal(n = 9, name = "Greys"),
    "Dark2" = RColorBrewer::brewer.pal(n = 8, name = "Dark2"),
    "Set1" = RColorBrewer::brewer.pal(n = 8, name = "Set1"),
    "Paired" = RColorBrewer::brewer.pal(n = 10, name = "Paired")#,
    # "viridis" = col2Hex(viridisLite::viridis(10)),
    # "magma" = col2Hex(viridisLite::magma(10)),
    # "inferno" = col2Hex(viridisLite::inferno(10))
  )

  # For palette picker

  colors_pal <- lapply(
    X = split(
      x = RColorBrewer::brewer.pal.info,
      f = factor(RColorBrewer::brewer.pal.info$category, labels = c("Diverging", "Qualitative", "Sequential"))
    ),
    FUN = rownames
  )
  background_pals <- sapply(unlist(colors_pal, use.names = FALSE), get_brewer_name)
  background_pals <- c(list("ggplot2" = scales::hue_pal()(9)), background_pals)
  # background_pals <- c(
  #   background_pals, list(
  #     "viridis" = col2Hex(viridisLite::viridis(10)),
  #     "magma" = col2Hex(viridisLite::magma(10)),
  #     "inferno" = col2Hex(viridisLite::inferno(10))
  #   )
  # )
  background_pals <- unlist(lapply(X = background_pals, FUN = linear_gradient))
  colortext_pals <- rep(c("white", "black", "black"), times = sapply(colors_pal, length))
  colortext_pals <- c("white", colortext_pals)
  # colortext_pals <- c(colortext_pals, c("white", "white", "white"))

  list(colors_pal = colors_pal,
       background_pals = background_pals,
       colortext_pals = colortext_pals,
       choices_colors = choices_colors)
}









