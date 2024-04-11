
#' Controls for theme
#'
#' Set color, palette, theme, legend position
#'
#' @param id Module ID.
#' @param style Custom CSS styles for the container.
#'
#' @noRd
#'
#' @importFrom utils head
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets radioGroupButtons colorPickr virtualSelectInput
controls_theme_ui <- function(id, style = NULL) {

  ns <- NS(id)

  themes <- get_themes()


  tags$div(
    class = "esquisse-controls-theme-container",
    style = style,
    shinyWidgets::virtualSelectInput(
      inputId = ns("theme"),
      label = i18n("Theme:"),
      choices = themes,
      selected = getOption("esquisse.default.theme", default = "theme_minimal"),
      dropboxWrapper = ".esquisse-controls-theme-container",
      optionsCount = 5,
      width = "100%"
    ),
    input_legend_options(ns)
  )
}


#' @importFrom shiny observeEvent observe req reactive
controls_theme_server <- function(id) {
  moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      inputs_r <- reactive({

        legend_position <- input$legend_position
        if (identical(legend_position, "right"))
          legend_position <- NULL

        legend_justification <- input$legend_justification
        if (identical(legend_justification, "center"))
          legend_justification <- NULL

        list(
          theme = input$theme,
          legend_position = legend_position,
          legend_justification = legend_justification,
          legend_text = get_axis_text(
            input$legend_text_face,
            input$legend_text_size,
            input$legend_text_angle
          ),
          legend_title = get_axis_text(
            input$legend_title_face,
            input$legend_title_size,
            input$legend_title_angle
          )
        )
      })

      return(list(inputs = inputs_r))
    }
  )
}


input_legend_text <- function(type = c("text", "title"), ns = identity) {
  type <- match.arg(type)
  tagList(
    tags$p(capitalize(type), "options:"),
    tags$div(
      style = css(
        display = "grid",
        gridTemplateColumns = "repeat(3, 1fr)",
        gridColumnGap = "2px"
      ),
      shinyWidgets::virtualSelectInput(
        inputId = ns(paste0("legend_", type, "_face")),
        label = "Font face:",
        choices = setNames(
          c("plain", "italic", "bold", "bold.italic"),
          c("Plain", "Italic", "Bold", "Bold/Italic")
        ),
        width = "100%"
      ),
      numericInput(
        inputId = ns(paste0("legend_", type, "_size")),
        label = "Size:",
        value = 10,
        min = 0,
        width = "100%"
      ),
      numericInput(
        inputId = ns(paste0("legend_", type, "_angle")),
        label = "Angle:",
        value = 0,
        min = 0,
        max = 360,
        width = "100%"
      )
    )
  )
}

input_legend_options <- function(ns) {
  tagList(
    tags$hr(),
    tags$b("Legend options:"),
    radioGroupButtons(
      inputId = ns("legend_position"),
      label = i18n("Position:"),
      choiceNames = list(
        ph("arrow-left", title = "left"),
        ph("arrow-up", title = "top"),
        ph("arrow-down", title = "bottom"),
        ph("arrow-right", title = "right"),
        ph("x", title = "none")
      ),
      choiceValues = c("left", "top", "bottom", "right", "none"),
      selected = "right",
      justified = TRUE,
      size = "sm"
    ),
    radioGroupButtons(
      inputId = ns("legend_justification"),
      label = i18n("Justification:"),
      choiceNames = list(
        ph("arrow-left", title = "left"),
        ph("arrow-up", title = "top"),
        ph("arrow-down", title = "bottom"),
        ph("arrow-right", title = "right"),
        ph("arrows-in-cardinal", title = "center")
      ),
      choiceValues = c("left", "top", "bottom", "right", "center"),
      selected = "center",
      justified = TRUE,
      size = "sm"
    ),
    input_legend_text("text", ns = ns),
    input_legend_text("title", ns = ns)
  )
}
