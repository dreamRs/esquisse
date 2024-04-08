
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
    # tags$div(
    #   id = ns("controls-scatter"),
    #   style = "display: none; padding-top: 10px;",
    #   tags$label(
    #     class = "control-label",
    #     `for` = ns("smooth_add"),
    #     i18n("Add a smooth line:")
    #   ),
    #   prettyToggle(
    #     inputId = ns("smooth_add"),
    #     label_on = i18n("Yes"),
    #     status_on = "success",
    #     status_off = "danger",
    #     label_off = i18n("No"),
    #     inline = TRUE
    #   ),
    #   conditionalPanel(
    #     condition = paste0("input.smooth_add==true"),
    #     ns = ns,
    #     sliderInput(
    #       inputId = ns("smooth_span"),
    #       label = i18n("Smooth line span:"),
    #       min = 0.1,
    #       max = 1,
    #       value = 0.75,
    #       step = 0.01,
    #       width = "100%"
    #     )
    #   ),
    # ),

    # tags$div(
    #   id = ns("controls-jitter"),
    #   style = "display: none; padding-top: 10px;",
    #   tags$label(
    #     class = "control-label",
    #     `for` = ns("jitter_add"),
    #     i18n("Jittered points:")
    #   ),
    #   prettyToggle(
    #     inputId = ns("jitter_add"),
    #     label_on = i18n("Yes"),
    #     status_on = "success",
    #     status_off = "danger",
    #     label_off = i18n("No"),
    #     inline = TRUE
    #   )
    # ),

    input_axis_text("x", ns = ns),
    input_axis_text("y", ns = ns),

    tags$div(
      id = ns("controls-scale-trans-x"), style = "display: none;",
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
    tags$div(
      id = ns("controls-scale-trans-y"), style = "display: none;",
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
    tags$label(
      class = "control-label",
      `for` = ns("flip"),
      i18n("Flip coordinate:")
    ),
    prettyToggle(
      inputId = ns("flip"),
      label_on = i18n("Yes"),
      status_on = "success",
      status_off = "danger",
      label_off = i18n("No"),
      inline = TRUE
    )
  )
}


controls_axes_server <- function(id,
                                 use_transX = reactive(FALSE),
                                 use_transY = reactive(FALSE),
                                 type = reactiveValues()) {
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



      smooth_r <- reactive({
        list(
          add = input$smooth_add,
          args = list(
            span = input$smooth_span
          )
        )
      })

      jitter_r <- reactive({
        list(
          add = input$jitter_add,
          args = list()
        )
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
        if (isTRUE(input$flip)) "flip" else NULL
      )

      limits_r <- reactive({
        list(
          x = use_transX() & !anyNA(input$xlim),
          xlim = input$xlim,
          y = use_transY() & !anyNA(input$ylim),
          ylim = input$ylim
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
        smooth = smooth_r,
        coord = coord_r,
        jitter = jitter_r,
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
    ),
    tags$hr()
  )
}




