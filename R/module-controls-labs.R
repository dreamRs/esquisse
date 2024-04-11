#' Controls for labs
#'
#' Set title, subtitle, caption, xlab, ylab
#'
#' @param id Module ID
#'
#' @noRd
#' @importFrom htmltools tagList tags
#'
controls_labs_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "form-group",
    labs_options_input(
      inputId = ns("labs_title"),
      placeholder = i18n("Title"),
      label = i18n("Title:"),
      defaults = get_labs_defaults("title")
    ),
    labs_options_input(
      inputId = ns("labs_subtitle"),
      placeholder = i18n("Subtitle"),
      label = i18n("Subtitle:"),
      defaults = get_labs_defaults("subtitle")
    ),
    labs_options_input(
      inputId = ns("labs_caption"),
      placeholder = i18n("Caption"),
      label = i18n("Caption:"),
      defaults = get_labs_defaults("caption")
    ),
    labs_options_input(
      inputId = ns("labs_x"),
      placeholder = i18n("X label"),
      label = i18n("X label:"),
      defaults = get_labs_defaults("x")
    ),
    labs_options_input(
      inputId = ns("labs_y"),
      placeholder = i18n("Y label"),
      label = i18n("Y label:"),
      defaults = get_labs_defaults("y")
    ),
    tags$div(
      id = ns("controls-labs-fill"),
      style = "display: none;",
      textInput(inputId = ns("labs_fill"), placeholder = i18n("Fill label"), label = i18n("Fill label:"))
    ),
    tags$div(
      id = ns("controls-labs-color"),
      style = "display: none;",
      textInput(inputId = ns("labs_color"), placeholder = i18n("Color label"), label = i18n("Color label:"))
    ),
    tags$div(
      id = ns("controls-labs-size"),
      style = "display: none;",
      textInput(inputId = ns("labs_size"), placeholder = i18n("Size label"), label = i18n("Size label:"))
    ),
    tags$div(
      id = ns("controls-labs-shape"),
      style = "display: none;",
      textInput(inputId = ns("labs_shape"), placeholder = i18n("Shape label"), label = i18n("Shape label:"))
    )
  )
}


controls_labs_server <- function(id,
                                 data_table = reactive(NULL),
                                 aesthetics_r = reactive(NULL)) {
  moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      # Reset labs ----
      observeEvent(data_table(), {
        updateTextInput(session = session, inputId = "labs_title", value = character(0))
        updateTextInput(session = session, inputId = "labs_subtitle", value = character(0))
        updateTextInput(session = session, inputId = "labs_caption", value = character(0))
        updateTextInput(session = session, inputId = "labs_x", value = character(0))
        updateTextInput(session = session, inputId = "labs_y", value = character(0))
        updateTextInput(session = session, inputId = "labs_fill", value = character(0))
        updateTextInput(session = session, inputId = "labs_color", value = character(0))
        updateTextInput(session = session, inputId = "labs_size", value = character(0))
        updateTextInput(session = session, inputId = "labs_shape", value = character(0))
      })

      # display specific control according to aesthetics set
      observeEvent(aesthetics_r(), {
        aesthetics <- names(aesthetics_r())
        toggleDisplay("controls-labs-fill", display = "fill" %in% aesthetics)
        toggleDisplay("controls-labs-color", display = "color" %in% aesthetics)
        toggleDisplay("controls-labs-size", display = "size" %in% aesthetics)
        toggleDisplay("controls-labs-shape", display = "shape" %in% aesthetics)
        toggleDisplay("controls-ribbon-color", display = "ymin" %in% aesthetics)
      })


      # labs input
      labs_r <- debounce(reactive({
        asth <- names(aesthetics_r())
        labs_fill <- `if`(isTRUE("fill" %in% asth), input$labs_fill, "")
        labs_color <- `if`(isTRUE("color" %in% asth), input$labs_color, "")
        labs_size <- `if`(isTRUE("size" %in% asth), input$labs_size, "")
        labs_shape <- `if`(isTRUE("shape" %in% asth), input$labs_shape, "")
        list(
          x = input$labs_x %empty% NULL,
          y = input$labs_y %empty% NULL,
          title = input$labs_title %empty% NULL,
          subtitle = input$labs_subtitle %empty% NULL,
          caption = input$labs_caption %empty% NULL,
          fill = labs_fill %empty% NULL,
          color = labs_color %empty% NULL,
          size = labs_size %empty% NULL,
          shape = labs_shape %empty% NULL
        )
      }), millis = 1000)


      theme_r <- reactive({
        inputs <- reactiveValuesToList(input)
        list(
          title = get_labs_options(inputs, "title"),
          subtitle = get_labs_options(inputs, "subtitle"),
          caption = get_labs_options(inputs, "caption"),
          x = get_labs_options(inputs, "x"),
          y = get_labs_options(inputs, "y")
        )
      })

      return(list(labs = labs_r, theme = theme_r))
    }
  )
}


#' @importFrom htmltools tags
#' @importFrom shinyWidgets dropMenu prettyRadioButtons
#' @importFrom shiny actionButton numericInput
labs_options_input <- function(inputId, label, placeholder, defaults = list()) {
  tags$div(
    class = "esquisse-labs",
    tags$label(
      class = "control-label",
      id = paste0(inputId, "-label"),
      `for` = inputId,
      label
    ),
    tags$div(
      class = "esquisse-labs-options",
      tags$div(
        class = "form-group shiny-input-container",
        style = "width: 100%;",
        tags$input(
          id = inputId,
          type = "text",
          class = "form-control",
          value = "",
          placeholder = placeholder,
          style = "border-radius: 4px 0 0 4px;"
        )
      ),
      dropMenu(
        actionButton(
          inputId = paste0(inputId, "_options"),
          label = ph("plus", title = "Options"),
          style = "border-radius: 0 4px 4px 0; width: 100%;",
          class = "btn-outline-primary border px-0"
        ),
        style = "width: 320px;",
        options = shinyWidgets::dropMenuOptions(flip = TRUE, preventOverflow = TRUE),
        prettyRadioButtons(
          inputId = paste0(inputId, "_face"),
          label = i18n("Font face:"),
          choiceNames = c("Plain", "Italic", "Bold", "Bold/Italic"),
          choiceValues = c("plain", "italic", "bold", "bold.italic"),
          selected = defaults$face,
          status = "primary",
          inline = TRUE
        ),
        numericInput(
          inputId = paste0(inputId, "_size"),
          label = i18n("Font size:"),
          value = defaults$size
        ),
        prettyRadioButtons(
          inputId = paste0(inputId, "_align"),
          label = i18n("Align:"),
          choiceNames = c(i18n("Left"), i18n("Center"), i18n("Right")),
          choiceValues = c("left", "center", "right"),
          inline = TRUE,
          status = "primary",
          selected = switch(
            as.character(defaults$hjust),
            "0" = "left",
            "0.5" = "center",
            "1" = "right"
          )
        ),
        placement = "right"
      )
    )
  )
}

get_labs_defaults <- function(name = c("title", "subtitle", "caption", "x", "y")) {
  name <- match.arg(name)
  defaults_labs <- list(
    title = list(size = 13L, face = "plain", hjust = 0), # theme_get()$plot.title
    subtitle = list(size = 11L, face = "plain", hjust = 0), # theme_get()$plot.subtitle
    caption = list(size = 9L, face = "plain", hjust = 1), # theme_get()$plot.caption
    x = list(size = 11L, face = "plain", hjust = 0.5),
    y = list(size = 11L, face = "plain", hjust = 0.5)
  )
  defaults_labs[[name]]
}

get_labs_options <- function(inputs, name = c("title", "subtitle", "caption", "x", "y")) {
  name <- match.arg(name)
  defaults <- get_labs_defaults(name)
  inputs <- inputs[paste0("labs_", name, c("_size", "_face", "_align"))]
  names(inputs) <- c("size", "face", "align")
  if (length(inputs$align) < 1) {
    inputs$hjust <- defaults$hjust
  } else {
    inputs$hjust <- switch(
      inputs$align,
      "left" = 0,
      "center" = 0.5,
      "right" = 1
    )
  }
  inputs$align <- NULL
  for (nm in names(defaults)) {
    if (identical(inputs[[nm]], defaults[[nm]])) {
      inputs[[nm]] <- NULL
    }
  }
  inputs <- dropNulls(inputs)
  if (length(inputs) > 0) {
    call2("element_text", !!!inputs)
  } else {
    NULL
  }
}
