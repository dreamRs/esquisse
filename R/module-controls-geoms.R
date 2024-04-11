
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

  tags$div(
    class = "esquisse-controls-geoms-container",
    style = style,
    tags$div(
      id = ns("controls-fill-color"),
      style = "display: block;",
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
      id = ns("controls-palette"),
      style = "display: none;",
      palette_ui(ns("colors"))
    ),
    tags$div(
      id = ns("controls-ribbon-color"),
      style = "display: none;",
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
      id = ns("controls-points"),
      style = "display: none;",
      sliderInput(
        inputId = ns("size"),
        label = i18n("Size for points:"),
        min = 0.5,
        max = 5,
        value = 1.5,
        width = "100%"
      ),
      virtualSelectInput(
        inputId = ns("shape"),
        label = "Shape:",
        choices = c(
          "circle", paste("circle", c("open", "filled", "cross", "plus", "small")), "bullet",
          "square", paste("square", c("open", "filled", "cross", "plus", "triangle")),
          "diamond", paste("diamond", c("open", "filled", "plus")),
          "triangle", paste("triangle", c("open", "filled", "square")),
          paste("triangle down", c("open", "filled")),
          "plus", "cross", "asterisk"
        ),
        width = "100%"
      )
    ),
    tags$div(
      id = ns("controls-lines"),
      style = "display: none;",
      sliderInput(
        inputId = ns("linewidth"),
        label = i18n("Line width:"),
        min = 0,
        max = 3,
        value = 0.5,
        step = 0.05,
        width = "100%"
      ),
      virtualSelectInput(
        inputId = ns("linetype"),
        label = "Line type:",
        choices = setNames(
          c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
          c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
        ),
        width = "100%"
      )
    ),
    tags$div(
      id = ns("controls-smooth"),
      style = "display: none; padding-top: 10px;",
      sliderInput(
        inputId = ns("span"),
        label = i18n("Controls the amount of smoothing:"),
        min = 0.1,
        max = 1,
        value = 0.75,
        step = 0.01,
        width = "100%"
      ),
      sliderInput(
        inputId = ns("level"),
        label = i18n("Level of confidence interval to use:"),
        min = 0.8,
        max = 1,
        value = 0.95,
        step = 0.01,
        width = "100%"
      )
    ),
    tags$div(
      id = ns("controls-histogram"),
      style = "display: none;",
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
      id = ns("controls-violin"),
      style = "display: none;",
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
                                  aesthetics_r = reactive(NULL),
                                  geoms_r = reactive(NULL))  {
  moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      bindEvent(observe({
        aesthetics <- names(aesthetics_r())
        geom <- geoms_r()
        toggleDisplay("controls-position", geom %in% c("bar", "line", "area", "histogram") & "fill" %in% aesthetics)
        toggleDisplay("controls-histogram", geom %in% "histogram")
        toggleDisplay("controls-density", geom %in% c("density", "violin"))
        toggleDisplay("controls-smooth", geom %in% "smooth")
        toggleDisplay("controls-points", geom %in% c("point"))
        toggleDisplay("controls-lines", geom %in% c("line", "step"))
        toggleDisplay("controls-violin", geom %in% "violin")
        toggleDisplay("controls-jitter", geom %in% c("boxplot", "violin"))
      }), geoms_r(), aesthetics_r())

      observeEvent(aesthetics_r(), {
        aesthetics <- dropNullsOrEmpty(aesthetics_r())
        cond <- !is.null(aesthetics$fill) | !is.null(aesthetics$color)
        toggleDisplay("controls-palette", display = isTRUE(cond))
        toggleDisplay("controls-fill-color", display = !isTRUE(cond))
      })

      inputs_r <- reactive({
        aesthetics <- names(aesthetics_r())

        dropNulls(list(
          adjust = input$adjust,
          position = input$position,
          size = if (!identical(input$size, 1.5)) input$size,
          linewidth = if (!identical(input$linewidth, 0.5)) input$linewidth,
          linetype = if (!identical(input$linetype, "solid")) input$linetype,
          fill_color = input$fill_color,
          color_ribbon = input$color_ribbon,
          shape = if (!identical(input$shape, "circle")) input$shape,
          span = if (!identical(input$span, 0.75)) input$span,
          level = if (!identical(input$level, 0.95)) input$level
        ))
      })

      # Colors input
      colors_r <- palette_server("colors", reactive({
        data_ <- data_table()
        aesthetics_ <- aesthetics_r()
        if ("fill" %in% names(aesthetics_)) {
          return(data_[[aesthetics_$fill]])
        }
        if ("color" %in% names(aesthetics_)) {
          return(data_[[aesthetics_$color]])
        }
        return(character(0))
      }))
      colors_r_d <- debounce(colors_r, millis = 1000)

      return(reactive(list(inputs = inputs_r(), colors = colors_r_d())))
    }
  )
}



# Multi geoms -------------------------------------------------------------

#' @importFrom bslib navset_hidden nav_panel_hidden
controls_multigeoms_ui <- function(id, style = NULL, n_geoms = 1) {
  ns <- NS(id)
  if (n_geoms == 1) {
    controls_geoms_ui(ns("geom1"), style = style)
  } else {
    navs_controls_geom <- lapply(
      X = seq_len(n_geoms),
      FUN = function(i) {
        nav_panel_hidden(
          value = paste0("geom", i),
          controls_geoms_ui(ns(paste0("geom", i)), style = style)
        )
      }
    )
    navset_hidden(
      id = ns("navset_controls_geoms"),
      !!!navs_controls_geom
    )
  }
}


#' @importFrom bslib nav_select
#' @importFrom shiny moduleServer observeEvent reactiveValues reactive
controls_multigeoms_server <- function(id,
                                  data_table = reactive(NULL),
                                  aesthetics_r = reactive(NULL),
                                  geoms_r = reactive(NULL),
                                  n_geoms = 1,
                                  active_geom_r = reactive("geom1"))  {
  moduleServer(
    id = id,
    function(input, output, session) {

      observeEvent(active_geom_r(), {
        nav_select(id = "navset_controls_geoms", selected = active_geom_r())
      })

      rv <- reactiveValues()

      lapply(
        X = seq_len(n_geoms),
        FUN = function(i) {

          res_r <- controls_geoms_server(
            id = paste0("geom", i),
            data_table = data_table,
            aesthetics_r = aesthetics_r,
            geoms_r = reactive({
              geoms_r()[i]
            })
          )

          observeEvent(res_r(), {
            rv[[paste0("geom", i)]] <- res_r()
          })

        }
      )

      return(reactive({
        lapply(
          X = seq_len(n_geoms),
          FUN = function(i) {
            list(
              inputs =  rv[[paste0("geom", i)]]$inputs,
              colors =  rv[[paste0("geom", i)]]$colors
            )
          }
        )
      }))
    }
  )
}



