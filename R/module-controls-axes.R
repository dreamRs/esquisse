
#' Controls for axes
#'
#'
#' @param id Module ID
#'
#' @noRd
#' @importFrom shiny sliderInput conditionalPanel selectInput numericInput
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets prettyRadioButtons numericRangeInput prettyToggle prettySwitch
#'
controls_axes_ui <- function(id) {

  ns <- NS(id)

  scales_trans <- c(
    "asn", "atanh", "boxcox", "exp", "identity",
    "log", "log10", "log1p", "log2", "logit",
    "probability", "probit", "reciprocal",
    "reverse", "sqrt"
  )

  tagList(

    input_axis_text("x", ns = ns),

    tags$div(
      id = ns("controls-scale-trans-x"),
      style = "display: none;",
      tags$b("X", "axis options:"),
      numericRangeInput(
        inputId = ns("xlim"),
        label = i18n("X-Axis limits (empty for none):"),
        value = c(NA, NA)
      ),
      selectInput(
        inputId = ns("transX"),
        label = i18n("X-Axis transform:"),
        selected = "identity",
        choices = scales_trans,
        width = "100%"
      )
    ),
    tags$hr(),

    input_axis_text("y", ns = ns),

    tags$div(
      id = ns("controls-scale-trans-y"),
      style = "display: none;",
      tags$b("Y", "axis options:"),
      numericRangeInput(
        inputId = ns("ylim"),
        label = i18n("Y-Axis limits (empty for none):"),
        value = c(NA, NA)
      ),
      selectInput(
        inputId = ns("transY"),
        label = i18n("Y-Axis transform:"),
        selected = "identity",
        choices = scales_trans,
        width = "100%"
      )
    ),
    tags$hr(),
    tags$b("Coordinate system:"),
    prettyRadioButtons(
      inputId = ns("coordinates"),
      label = "Coordinates:",
      choiceNames = c("Cartesian", "Flip", "Fixed", "Polar"),
      choiceValues = c("cartesian", "flip", "fixed", "polar"),
      status = "primary",
      outline = TRUE,
      inline = TRUE
    ),
    conditionalPanel(
      condition = "input.coordinates == 'fixed'",
      ns = ns,
      numericInput(
        inputId = ns("fixed_ratio"),
        label = "Aspect ratio:",
        value = 1,
        width = "100%"
      )
    ),
    conditionalPanel(
      condition = "input.coordinates == 'polar'",
      ns = ns,
      prettyRadioButtons(
        inputId = ns("polar_theta"),
        label = "Variable to map angle to:",
        choices = c("x", "y"),
        inline = TRUE,
        width = "100%"
      )
    )
  )
}


controls_axes_server <- function(id,
                                 use_transX = reactive(FALSE),
                                 use_transY = reactive(FALSE)) {
  moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      observeEvent(use_transX(), {
        toggleDisplay("controls-scale-trans-x", display = isTRUE(use_transX()))
      })

      observeEvent(use_transY(), {
        toggleDisplay("controls-scale-trans-y", display = isTRUE(use_transY()))
      })


      transX_r <- reactive({
        list(
          use = use_transX() & !identical(input$transX, "identity"),
          args = list(
            trans = input$transX
          )
        )
      })

      transY_r <- reactive({
        list(
          use = use_transY() & !identical(input$transY, "identity"),
          args = list(
            trans = input$transY
          )
        )
      })

      coord_r <- reactive(
        list(
          fun = if (!identical(input$coordinates, "cartesian")) input$coordinates,
          args = dropNulls(list(
            ratio = if (identical(input$coordinates, "fixed")) input$fixed_ratio,
            theta = if (identical(input$coordinates, "polar")) input$polar_theta
          ))
        )
      )

      limits_r <- reactive({
        list(
          xlim = if (use_transX() & !anyNA(input$xlim)) input$xlim,
          ylim = if (use_transY() & !anyNA(input$ylim)) input$ylim
        )
      })

      inputs_r <- reactive({
        list(
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
          )
        )
      })

      return(list(
        inputs = inputs_r,
        coord = coord_r,
        transX = transX_r,
        transY = transY_r,
        limits = limits_r
      ))

    }
  )
}





get_axis_text <- function(face, size, angle, hjust = 0, vjust = 0, lineheight = 1) {
  options <- dropNulls(list(
    face = if (isTRUE(face != "plain")) face,
    size = if (isTRUE(size != 10)) size,
    angle = if (isTRUE(angle != 0)) angle,
    hjust = if (isTRUE(hjust != 0)) hjust,
    vjust = if (isTRUE(vjust != 0)) vjust,
    lineheight = if (isTRUE(lineheight != 1)) lineheight
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
      )
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




