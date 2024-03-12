
#' Controls for appearance
#'
#' Set color, palette, theme, legend position
#'
#' @param ns Namespace from module
#'
#' @noRd
#'
#' @importFrom utils head
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets radioGroupButtons colorPickr virtualSelectInput
controls_appearance_ui <- function(id, style = NULL) {

  ns <- NS(id)

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

  tags$div(
    class = "esquisse-controls-appearance-container",
    style = style,
    tags$div(
      id = ns("controls-fill-color"), style = "display: block;",
      shinyWidgets::colorPickr(
        inputId = ns("fill_color"),
        label = i18n("Color:"),
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
        label = i18n("Ribbon color:"),
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
      shinyWidgets::virtualSelectInput(
        inputId = ns("shape"),
        label = i18n("Point symbol:"),
        choices = shape_names,
        selected = "circle",
        dropboxWrapper = ".esquisse-controls-appearance-container",
        optionsCount = 5,
        width = "100%"
      )
    ),
    shinyWidgets::virtualSelectInput(
      inputId = ns("theme"),
      label = i18n("Theme:"),
      choices = themes,
      selected = getOption("esquisse.default.theme", default = "theme_minimal"),
      dropboxWrapper = ".esquisse-controls-appearance-container",
      optionsCount = 5,
      width = "100%"
    ),
    radioGroupButtons(
      inputId = ns("legend_position"),
      label = i18n("Legend position:"),
      choiceNames = list(
        ph("arrow-left"),
        ph("arrow-up"),
        ph("arrow-down"),
        ph("arrow-right"),
        ph("x")
      ),
      choiceValues = c("left", "top", "bottom", "right", "none"),
      selected = "right",
      justified = TRUE,
      size = "sm"
    ),
    radioGroupButtons(
      inputId = ns("legend_justification"),
      label = i18n("Legend justification:"),
      choiceNames = list(
        ph("arrow-left"),
        ph("arrow-up"),
        ph("arrow-down"),
        ph("arrow-right"),
        ph("arrows-in-cardinal")
      ),
      choiceValues = c("left", "top", "bottom", "right", "center"),
      selected = "center",
      justified = TRUE,
      size = "sm"
    ),
    input_axis_text("x", ns = ns),
    input_axis_text("y", ns = ns)
  )
}



controls_appearance_server <- function(id,
                                       data_table = reactive(NULL),
                                       aesthetics = reactive(NULL),
                                       type = reactiveValues())  {
  moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      observeEvent(type$palette, {
        toggleDisplay("controls-palette", display = isTRUE(type$palette))
        toggleDisplay("controls-fill-color", display = !isTRUE(type$palette))
      })

      observe({
        req(aesthetics())
        aesthetics <- names(aesthetics())
        toggleDisplay("controls-shape", display = type$controls %in% "point" & !"shape" %in% aesthetics)
      })

      inputs_r <- reactive({
        aesthetics <- names(aesthetics())

        shape <- input$shape
        if (!(type$controls %in% "point" & !"shape" %in% aesthetics))
          shape <- NULL

        legend_position <- input$legend_position
        if (identical(legend_position, "right"))
          legend_position <- NULL

        legend_justification <- input$legend_justification
        if (identical(legend_justification, "center"))
          legend_justification <- NULL

        list(
          fill_color = input$fill_color,
          color_ribbon = input$color_ribbon,
          theme = input$theme,
          legend_position = legend_position,
          legend_justification = legend_justification,
          axis_text_x = get_axis_text(
            input$x_axis_text_face,
            input$x_axis_text_size,
            input$x_axis_text_angle,
            input$x_axis_text_hjust,
            input$x_axis_text_vjust
          ),
          axis_text_y = get_axis_text(
            input$y_axis_text_face,
            input$y_axis_text_size,
            input$y_axis_text_angle,
            input$y_axis_text_hjust,
            input$y_axis_text_vjust
          ),
          shape = shape
        )
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

      return(list(inputs = inputs_r, colors = colors_r_d))
    }
  )
}



get_axis_text <- function(face, size, angle, hjust, vjust, lineheight = 1) {
  options <- dropNulls(list(
    face = if (face != "plain") face,
    size = if (size != 10) size,
    angle = if (angle != 0) angle,
    hjust = if (hjust != 0) hjust,
    vjust = if (vjust != 0) vjust,
    lineheight = if (lineheight != 1) lineheight
  ))
  if (length(options) > 0) {
    call2("element_text", !!!options)
  } else {
    NULL
  }
}


input_axis_text <- function(axis = c("x", "y"), ns = identity) {
  axis <- match.arg(axis)
  tagList(
    tags$b(toupper(axis), "axis text options:"),
    tags$div(
      style = css(
        display = "grid",
        gridTemplateColumns = "repeat(3, 1fr)",
        gridColumnGap = "2px"
      ),
      shinyWidgets::virtualSelectInput(
        inputId = ns(paste0(axis, "_axis_text_face")),
        label = "Font face:",
        choices = setNames(
          c("plain", "italic", "bold", "bold.italic"),
          c("Plain", "Italic", "Bold", "Bold/Italic")
        ),
        width = "100%"
      ),
      numericInput(
        inputId = ns(paste0(axis, "_axis_text_size")),
        label = "Size:",
        value = 10,
        min = 0,
        width = "100%"
      ),
      numericInput(
        inputId = ns(paste0(axis, "_axis_text_angle")),
        label = "Angle:",
        value = 0,
        min = 0,
        max = 360,
        width = "100%"
      )
    ),
    tags$div(
      style = css(
        display = "grid",
        gridTemplateColumns = "repeat(2, 1fr)",
        gridColumnGap = "2px"
      ),
      numericInput(
        inputId = ns(paste0(axis, "_axis_text_hjust")),
        label = "Horizontal justification:",
        value = 0,
        min = 0,
        step = 0.1,
        max = 1,
        width = "100%"
      ),
      numericInput(
        inputId = ns(paste0(axis, "_axis_text_vjust")),
        label = "Vertical justification:",
        value = 0,
        min = 0,
        step = 0.1,
        max = 1,
        width = "100%"
      )#,
      # numericInput(
      #   inputId = ns(paste0(axis, "_axis_text_lineheight")),
      #   label = "Line height:",
      #   value = 1,
      #   step = 0.1,
      #   width = "100%"
      # )
    )
  )
}

