
#' Controls for general options
#'
#' Set bins for histogram, position for barchart, flip coordinates
#'
#' @param id Module ID
#'
#' @noRd
#' @importFrom shiny sliderInput conditionalPanel selectInput numericInput
#' @importFrom htmltools tagList tags
#' @importFrom shinyWidgets prettyRadioButtons numericRangeInput prettyToggle prettySwitch
#'
controls_options_ui <- function(id) {

  ns <- NS(id)

  tagList(
    tags$b("Dimension:"),
    numericInputIcon(
      inputId = ns("width"),
      label = NULL,
      value = NA,
      icon = list(i18n("Width:")),
      width = "100%"
    ),
    numericInputIcon(
      inputId = ns("height"),
      label = NULL,
      value = NA,
      icon = list(i18n("Height:")),
      width = "100%"
    ),
    prettySwitch(
      inputId = ns("plotly"),
      label = i18n("Use {plotly} to render plot"),
      fill = TRUE,
      status = "primary"
    ),
    tags$div(
      id = ns("controls-facet"), style = "display: none;",
      tags$b("Facets:"),
      prettyRadioButtons(
        inputId = ns("facet_scales"),
        label = i18n("Facet scales:"),
        inline = TRUE,
        status = "primary",
        choices = c("fixed", "free", "free_x", "free_y"),
        outline = TRUE
      ),
      sliderInput(
        inputId = ns("facet_ncol"),
        label = i18n("Facet ncol:"),
        min = 0,
        max = 10,
        value = 0,
        step = 1
      ),
      sliderInput(
        inputId = ns("facet_nrow"),
        label = i18n("Facet nrow:"),
        min = 0,
        max = 10,
        value = 0,
        step = 1
      )
    )
  )
}


controls_options_server <- function(id,
                                    use_facet = reactive(FALSE),
                                    width = reactive(NULL),
                                    height = reactive(NULL)) {
  moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      observeEvent(width(), {
        # print(width())
        updateNumericInputIcon(
          session = session,
          inputId = "width",
          value = width()
        )
      })

      observeEvent(height(), {
        # print(height())
        updateNumericInputIcon(
          session = session,
          inputId = "height",
          value = height()
        )
      })

      observeEvent(use_facet(), {
        toggleDisplay("controls-facet", display = isTRUE(use_facet()))
      })

      facet_r <- reactive({
        list(
          scales = if (identical(input$facet_scales, "fixed")) NULL else input$facet_scales,
          ncol = if (is.null(input$facet_ncol) || input$facet_ncol == 0) {
            NULL
          } else {
            input$facet_ncol
          },
          nrow = if (is.null(input$facet_ncol) || input$facet_nrow == 0) {
            NULL
          } else {
            input$facet_nrow
          }
        )
      })


      return(list(
        facet = facet_r,
        width = debounce(reactive(input$width), 800),
        height = debounce(reactive(input$height), 800),
        plotly = reactive(input$plotly)
      ))

    }
  )
}


