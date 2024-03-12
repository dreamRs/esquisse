
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
#' @importFrom shinyWidgets pickerInput radioGroupButtons colorPickr
controls_appearance_ui <- function(id) {

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

  tagList(
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
      pickerInput(
        inputId = ns("shape"),
        label = i18n("Point symbol:"),
        choices = shape_names,
        selected = "circle",
        options = list(size = 10, container = "body"),
        width = "100%"
      )
    ),
    pickerInput(
      inputId = ns("theme"),
      label = i18n("Theme:"),
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
    )
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
        toggleDisplay(id = ns("controls-palette"), display = isTRUE(type$palette))
        toggleDisplay(id = ns("controls-fill-color"), display = !isTRUE(type$palette))
      })

      observe({
        req(aesthetics())
        aesthetics <- names(aesthetics())
        toggleDisplay(id = ns("controls-shape"), display = type$controls %in% "point" & !"shape" %in% aesthetics)
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
