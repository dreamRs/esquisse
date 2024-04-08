
#' Controls for geoms
#'
#' Set color, palette, theme, legend position
#'
#' @param id Module ID.
#'
#' @noRd
#'
#' @importFrom utils head
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets radioGroupButtons colorPickr virtualSelectInput
controls_geoms_ui <- function(id, style = NULL) {

  ns <- NS(id)

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
    class = "esquisse-controls-geoms-container",
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
      id = ns("controls-size"), style = "display: none;",
      sliderInput(
        inputId = ns("size"),
        label = i18n("Size for points/lines:"),
        min = 0.5,
        max = 5,
        value = 1.2,
        width = "100%"
      )
    ),
    tags$div(
      id = ns("controls-histogram"), style = "display: none;",
      sliderInput(
        inputId = ns("bins"),
        label = i18n("Numbers of bins:"),
        min = 10,
        max = 100,
        value = 30,
        width = "100%"
      )
    ),
    tags$div(
      id = ns("controls-violin"), style = "display: none;",
      prettyRadioButtons(
        inputId = ns("scale"),
        label = i18n("Scale:"),
        inline = TRUE,
        status = "primary",
        choices = c("area", "count", "width"),
        outline = TRUE
      )
    ),
    tags$div(
      id = ns("controls-density"),
      style = "display: none;",
      sliderInput(
        inputId = ns("adjust"),
        label = i18n("Bandwidth adjustment:"),
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
        label = i18n("Position:"),
        choices = c("stack", "dodge", "fill"),
        inline = TRUE,
        selected = "stack",
        status = "primary",
        outline = TRUE
      )
    )
  )
}


#' @importFrom shiny observeEvent observe req reactive bindEvent
controls_geoms_server <- function(id,
                                  data_table = reactive(NULL),
                                  aesthetics = reactive(NULL),
                                  type = reactiveValues())  {
  moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      bindEvent(observe({
        aesthetics <- names(aesthetics())
        toggleDisplay("controls-position", type$controls %in% c("bar", "line", "area", "histogram") & "fill" %in% aesthetics)
        toggleDisplay("controls-histogram", type$controls %in% "histogram")
        toggleDisplay("controls-density", type$controls %in% c("density", "violin"))
        toggleDisplay("controls-scatter", type$controls %in% "point")
        toggleDisplay("controls-size", type$controls %in% c("point", "line", "step", "sf"))
        toggleDisplay("controls-violin", type$controls %in% "violin")
        toggleDisplay("controls-jitter", type$controls %in% c("boxplot", "violin"))

        if (type$controls %in% c("point")) {
          updateSliderInput(session = session, inputId = "size", value = 1.5)
        } else if (type$controls %in% c("line", "step")) {
          updateSliderInput(session = session, inputId = "size", value = 0.5)
        }
      }), type$controls, aesthetics())

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

        list(
          adjust = input$adjust,
          position = input$position,
          size = input$size,
          linewidth = input$size,
          fill_color = input$fill_color,
          color_ribbon = input$color_ribbon,
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


