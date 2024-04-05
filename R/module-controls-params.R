
#' Controls for parameters
#'
#' Set bins for histogram, position for barchart, flip coordinates
#'
#' @param ns Namespace from module
#'
#' @noRd
#' @importFrom shiny sliderInput conditionalPanel selectInput numericInput
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets prettyRadioButtons numericRangeInput prettyToggle prettySwitch
#'
controls_parameters_ui <- function(id) {

  ns <- NS(id)

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
        i18n("Add a smooth line:")
      ),
      prettyToggle(
        inputId = ns("smooth_add"),
        label_on = i18n("Yes"),
        status_on = "success",
        status_off = "danger",
        label_off = i18n("No"),
        inline = TRUE
      ),
      conditionalPanel(
        condition = paste0("input.smooth_add==true"),
        ns = ns,
        sliderInput(
          inputId = ns("smooth_span"),
          label = i18n("Smooth line span:"),
          min = 0.1,
          max = 1,
          value = 0.75,
          step = 0.01,
          width = "100%"
        )
      ),
    ),
    tags$div(
      id = ns("controls-jitter"),
      style = "display: none; padding-top: 10px;",
      tags$label(
        class = "control-label",
        `for` = ns("jitter_add"),
        i18n("Jittered points:")
      ),
      prettyToggle(
        inputId = ns("jitter_add"),
        label_on = i18n("Yes"),
        status_on = "success",
        status_off = "danger",
        label_off = i18n("No"),
        inline = TRUE
      )
    ),
    tags$div(
      id = ns("controls-size"), style = "display: none;",
      sliderInput(
        inputId = ns("size"),
        label = i18n("Size for points/lines:"),
        min = 0.5,
        max = 4,
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


controls_parameters_server <- function(id,
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


      observeEvent(type$controls, {
        toggleDisplay("controls-position", display = type$controls %in% c("bar", "line", "area", "histogram"))
        toggleDisplay("controls-histogram", display = type$controls %in% "histogram")
        toggleDisplay("controls-density", display = type$controls %in% c("density", "violin"))
        toggleDisplay("controls-scatter", display = type$controls %in% "point")
        toggleDisplay("controls-size", display = type$controls %in% c("point", "line", "step", "sf"))
        toggleDisplay("controls-violin", display = type$controls %in% "violin")
        toggleDisplay("controls-jitter", display = type$controls %in% c("boxplot", "violin"))

        if (type$controls %in% c("point")) {
          updateSliderInput(session = session, inputId = "size", value = 1.5)
        } else if (type$controls %in% c("line", "step")) {
          updateSliderInput(session = session, inputId = "size", value = 0.5)
        }
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

      return(list(
        smooth = smooth_r,
        coord = coord_r,
        jitter = jitter_r,
        transX = transX_r,
        transY = transY_r,
        limits = limits_r,
        inputs = reactive({list(
          position = input$position,
          size = input$size,
          linewidth = input$size
        )})
      ))

    }
  )
}


