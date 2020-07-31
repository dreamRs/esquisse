
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
#' @importFrom htmltools tagAppendAttributes singleton tags
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
  colPicTag <- pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected, 
    multiple = multiple,
    choicesOpt = dropNulls(list(
      style = style,
      content = sprintf(
        content_str,
        cols, 
        rep_len(textColor, length.out = length(cols)), 
        colsNames
      )
    )),
    options = pickerOpts,
    width = width
  )
  colPicTag <- tagAppendAttributes(
    tag = colPicTag, 
    class = ifelse(isTRUE(plainColor), "color-picker-plain", "color-picker")
  )
  tagList(
    singleton(tags$head(tags$style(
      ".color-picker .bootstrap-select .dropdown-menu li a span.text {width: 100%;}"
    ))),
    colPicTag
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
  palPicTag <- pickerInput(
    inputId = inputId,
    label = label,
    choices = choicesNames,
    selected = selected, 
    choicesOpt = dropNulls(list(
      style = style,
      content = sprintf(
        content_str,
        choicesColors, 
        rep_len(textColor, length.out = length(choicesColors)), 
        unlist(choicesNames, recursive = TRUE, use.names = FALSE)
      )
    )),
    options = pickerOpts,
    width = width
  )
  palPicTag <- tagAppendAttributes(
    tag = palPicTag, 
    class = ifelse(isTRUE(plainColor), "color-picker-plain", "color-picker")
  )
  tagList(
    singleton(tags$head(tags$style(
      ".color-picker .bootstrap-select .dropdown-menu li a span.text {width: 100%;}",
      ".color-picker-plain .bootstrap-select .dropdown-menu li a span.text div {background:rgba(0,0,0,0) !important;}"
    ))),
    palPicTag
  )
}



