
#' @title Picker input to select color(s) or palette
#' 
#' @description Select menu to view and choose a color or a palette of colors.
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label Display label for the control, or \code{NULL} for no label.
#' @param choices List of values to select from. Values must be valid Hex colors.
#'  If elements of the list are named then that name rather than the value is displayed to the user.
#' @param selected The initially selected value (or multiple values if \code{multiple = TRUE}).
#'  If not specified then defaults to the first value for single-select lists and no values for multiple select lists.
#' @param textColor Color of the text displayed above colors, can be a vector of the same length as choices.
#' @param plainColor Color the full space of the choice menu.
#' @param multiple Is selection of multiple items allowed?
#' @param pickerOpts Options for \code{\link[shinyWidgets]{pickerInput}}.
#' @param width The width of the input : \code{'auto'}, \code{'fit'}, \code{'100px'}, \code{'75\%'}.
#'
#' @return A select control that can be added to a UI definition.
#' @export
#' 
#' @name input-colors
#' 
#' @importFrom htmltools tagAppendAttributes tags
#' @importFrom shinyWidgets pickerInput
#'
#' @example examples/colorPicker.R
colorPicker <- function(inputId, 
                        label, 
                        choices, 
                        selected = NULL,
                        textColor = "#000", 
                        plainColor = FALSE,
                        multiple = FALSE, 
                        pickerOpts = list(),
                        width = NULL) {
  opts <- colorPickerOptions(choices, textColor, plainColor, multiple)
  colPicTag <- pickerInput(
    inputId = inputId,
    label = label,
    choices = opts$choices,
    selected = selected, 
    multiple = multiple,
    choicesOpt = opts$choicesOpt,
    options = pickerOpts,
    width = width
  )
  colPicTag <- tagAppendAttributes(
    tag = colPicTag, 
    class = ifelse(isTRUE(plainColor), "color-picker-plain", "color-picker")
  )
  tagList(
    html_dependency_esquisse(),
    colPicTag
  )
}

#' @param session Shiny session.
#' 
#' @rdname input-colors
#' @export
#' 
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom shiny getDefaultReactiveDomain
updateColorPicker <- function(session = getDefaultReactiveDomain(),
                              inputId, 
                              choices,
                              textColor = "#000", 
                              plainColor = FALSE,
                              multiple = FALSE) {
  opts <- colorPickerOptions(choices, textColor, plainColor, multiple)
  updatePickerInput(
    session = session,
    inputId = inputId,
    choices = opts$choices,
    choicesOpt = opts$choicesOpt
  )
}


colorPickerOptions <- function(choices, textColor, plainColor, multiple) {
  choices <- choicesWithNames(choices)
  cols <- unlist(x = choices, recursive = TRUE, use.names = FALSE)
  colsNames <- unlist(lapply(
    X = seq_along(choices), 
    FUN = function(x) {
      if (is.list(choices[[x]])) {
        names(choices[[x]])
      } else {
        names(choices)[x]
      }
    }
  ))
  if (isTRUE(plainColor)) {
    style <- sprintf(
      "background: %s; color: %s;", 
      cols, rep_len(textColor, length.out = length(cols))
    )
  } else {
    style <- NULL
  }
  if (isTRUE(multiple)) {
    content_str <- "<span style='border-radius:4px; padding: 2px;background:%s;color:%s'>%s</span>"
  } else {
    content_str <- "<div style='width:100%%;border-radius:4px; padding: 2px;background:%s;color:%s'>%s</div>"
  }
  list(
    choices = choices,
    choicesOpt = dropNulls(list(
      style = style,
      content = sprintf(
        content_str,
        cols, 
        rep_len(textColor, length.out = length(cols)), 
        colsNames
      )
    ))
  )
}


#' @rdname input-colors
#' @export
#' 
#' @example examples/palettePicker.R
palettePicker <- function(inputId, 
                          label, 
                          choices, 
                          selected = NULL, 
                          textColor = "#000", 
                          plainColor = FALSE, 
                          pickerOpts = list(), 
                          width = NULL) {
  opts <- palettePickerOptions(choices, textColor, plainColor)
  palPicTag <- pickerInput(
    inputId = inputId,
    label = label,
    choices = opts$choices,
    selected = selected, 
    choicesOpt = opts$choicesOpt,
    options = pickerOpts,
    width = width
  )
  palPicTag <- tagAppendAttributes(
    tag = palPicTag, 
    class = ifelse(isTRUE(plainColor), "color-picker-plain", "color-picker")
  )
  tagList(
    html_dependency_esquisse(),
    palPicTag
  )
}

#' @param session Shiny session.
#' 
#' @rdname input-colors
#' @export
#' 
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom shiny getDefaultReactiveDomain
updatePalettePicker <- function(session = getDefaultReactiveDomain(),
                                inputId, 
                                choices,
                                selected = NULL,
                                textColor = "#000", 
                                plainColor = FALSE) {
  opts <- palettePickerOptions(choices, textColor, plainColor)
  updatePickerInput(
    session = session,
    inputId = inputId,
    selected = selected,
    choices = opts$choices,
    choicesOpt = opts$choicesOpt
  )
}

palettePickerOptions <- function(choices, textColor, plainColor) {
  choicesNames <- lapply(
    X = seq_along(choices), 
    FUN = function(x) {
      if (is.list(choices[[x]])) {
        names(choices[[x]])
      } else {
        names(choices)[x]
      }
    }
  )
  names(choicesNames) <- names(choices)
  choicesColors <- lapply(
    X = seq_along(choices), 
    FUN = function(x) {
      if (is.list(choices[[x]])) {
        lapply(choices[[x]], linear_gradient)
      } else {
        linear_gradient(choices[[x]])
      }
    }
  )
  choicesColors <- unlist(
    x = choicesColors, 
    recursive = TRUE, 
    use.names = FALSE
  )
  if (isTRUE(plainColor)) {
    style <- sprintf(
      "background: %s; color: %s;", 
      choicesColors, rep_len(textColor, length.out = length(choicesColors))
    )
  } else {
    style <- NULL
  }
  content_str <- "<div style='width:100%%;border-radius:4px; padding: 2px;background:%s;color:%s'>%s</div>"
  
  list(
    choices = choicesNames,
    choicesOpt = dropNulls(list(
      style = style,
      content = sprintf(
        content_str,
        choicesColors, 
        rep_len(textColor, length.out = length(choicesColors)), 
        unlist(choicesNames, recursive = TRUE, use.names = FALSE)
      )
    ))
  )
}




#' @importFrom htmltools tagList tags
#' @importFrom shiny NS uiOutput radioButtons checkboxInput
palette_ui <- function(id) {
  ns <- NS(id)
  pals <- get_palettes()
  tagList(
    tags$style(
      ".bootstrap-select .dropdown-menu li a span.text { display: block !important; }"
    ),
    radioButtons(
      inputId = ns("type"),
      label = NULL,
      choiceValues = c("palette", "manual"),
      choiceNames = tagList(
        tags$div(
          tags$b("Use a palette:"),
          palettePicker(
            inputId = ns("palette"),
            label = NULL,
            choices = pals$choices,
            textColor = pals$textColor,
            pickerOpts = list(container = "body")
          ),
          checkboxInput(
            inputId = ns("reverse"),
            value = FALSE,
            label = "Reverse colors?"
          )
        ),
        tags$div(
          tags$b("Use specific colors:"),
          uiOutput(outputId = ns("manual"))
        )
      )
    )
  )
}

#' @importFrom shinyWidgets colorPickr
#' @importFrom htmltools tagList tags tagAppendAttributes
#' @importFrom shiny callModule reactiveValues renderUI reactive isolate
#' @importFrom grDevices colorRampPalette
#' @importFrom scales seq_gradient_pal
palette_server <- function(id, variable) {
  
  palettes <- get_palettes()
  palettes <- palettes$choices
  palettes <- unlist(palettes, recursive = FALSE, use.names = TRUE)
  names(palettes) <- gsub("^.+\\.", "", names(palettes))
  
  callModule(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      colors_manual <- reactiveValues(x = NULL)
      output$manual <- renderUI({
        variable_ <- variable()
        req(variable_)
        type <- col_type(variable_)
        if (identical(type, "discrete")) {
          values <- sort(unique(variable_))
          colors <- colorRampPalette(palettes[[input$palette]])(length(values))
          colors_id <- paste0("colors_", makeId(values))
          colors_manual$x <- setNames(as.list(colors_id), values)
          colors_manual$type <- "discrete"
          lapply(
            X = seq_along(values),
            FUN = function(i) {
              tagList(
                tags$span(
                  tagAppendAttributes(
                    colorPickr(
                      inputId = ns(colors_id[i]),
                      selected = colors[i],
                      label = NULL,
                      theme = "classic",
                      useAsButton = TRUE,
                      update = "save",
                      interaction = list(
                        hex = FALSE,
                        rgba = FALSE,
                        input = TRUE,
                        save = TRUE,
                        clear = FALSE
                      )
                    ),
                    style = "display: inline; vertical-align: middle;"
                  ),
                  values[i]
                ),
                tags$br()
              )
            }
          )
        } else if (identical(type, "continuous")) {
          colors <- palettes[[input$palette]]
          if (identical(input$palette, "ggplot2")) {
            colors <- c("#132B43", "#56B1F7")
          }
          colors_manual$x <- list(low = "low", high = "high")
          colors_manual$type <- "continuous"
          tagList(
            tags$span(
              tagAppendAttributes(
                colorPickr(
                  inputId = ns("low"),
                  selected = colors[1],
                  label = NULL,
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
                ),
                style = "display: inline; vertical-align: middle;"
              ),
              "Low value"
            ),
            tags$div(style = "height: 5px;"),
            tags$span(
              tagAppendAttributes(
                colorPickr(
                  inputId = ns("high"),
                  selected = colors[length(colors)],
                  label = NULL,
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
                ),
                style = "display: inline; vertical-align: middle;"
              ),
              "High value"
            )
          )
        } else {
          tags$i(
            style = "color: #FAFAFA;",
            "No manual colors possible"
          )
        }
      })
      
      observeEvent(colors_manual$type, {
        pals <- get_palettes()
        if (identical(colors_manual$type, "continuous")) {
          if (!is.null(pals$choices$Default$ggplot2)) {
            x <- seq(0, 1, length.out = 10)
            pals$choices$Default$ggplot2 <- seq_gradient_pal("#132B43", "#56B1F7", "Lab")(x)
          }
        }
        updatePalettePicker(
          inputId = "palette",
          choices = pals$choices,
          textColor = pals$textColor, 
          selected = isolate(input$palette)
        )
      }, ignoreInit = TRUE)
      
      return(reactive({
        if (identical(input$type, "palette")) {
          list(
            scale = "palette",
            reverse = input$reverse,
            colors = input$palette
          )
        } else {
          ids <- colors_manual$x
          list(
            scale = "manual",
            type = colors_manual$type,
            colors = lapply(
              X = ids,
              FUN = function(x) {
                input[[x]]
              }
            )
          )
        }
      }))
    }
  )
}









