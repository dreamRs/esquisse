

#' Drag And Drop Input Widget
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label Display label for the control, or \code{NULL} for no label.
#' @param sourceLabel Label display in the source box
#' @param targetsLabels Labels for each target element.
#' @param targetsIds Ids for retrieving values server-side, if \code{NULL}, the default,
#'  \code{targetsLabels} are used after removing all not-alphanumeric characters.
#' @param choices List of values to select from (if elements of the list are
#'  named then that name rather than the value is displayed to the user).
#'  If this argument is provided, then \code{choiceNames} and \code{choiceValues} must
#'  not be provided, and vice-versa. The values should be strings; other
#'  types (such as logicals and numbers) will be coerced to strings.
#' @param choiceNames,choiceValues List of names and values, respectively,
#' that are displayed to the user in the app and correspond to the each
#' choice (for this reason, choiceNames and choiceValues must have the same length).
#' If either of these arguments is provided, then the other must be provided and
#' choices must not be provided. The advantage of using both of these over a named
#' list for choices is that choiceNames allows any type of UI object to be passed
#' through (tag objects, icons, HTML code, ...), instead of just simple text.
#' @param selected Default selected values. Must be a \code{list} with \code{targetsIds} as names.
#' @param badge Displays choices inside a Bootstrap badge. Use \code{FALSE}
#'  if you want to pass custom appearance with \code{choiceNames}.
#' @param ncolSource Number of columns occupied by the source, default is \code{"auto"}, meaning full row.
#' @param ncolGrid Number of columns used to place source and targets boxes, see examples.
#' @param status If choices are displayed into a Bootstrap label, you can use Bootstrap
#'  status to color them, or \code{NULL}.
#' @param replace When a choice is dragged in a target container already
#'  containing a choice, does the later be replaced by the new one ?
#' @param dragulaOpts Options passed to dragula JavaScript library.
#' @param boxStyle CSS style string to customize source and target container.
#' @param width Width of the input.
#' @param height Height of each boxes, the total input height is this parameter X 2.
#'
#' @note The output server-side is a list with two slots: \code{source} and \code{targets}.
#'
#' @seealso \code{\link{updateDragulaInput}} to update choices server-side.
#'
#' @return a UI definition
#' @export
#'
#' @importFrom htmltools validateCssUnit singleton tags
#' @importFrom jsonlite toJSON
#' @importFrom shiny fillRow splitLayout
#'
#' @example examples/dragulaInput.R
dragulaInput <- function(inputId,
                         label = NULL,
                         sourceLabel,
                         targetsLabels,
                         targetsIds = NULL,
                         choices = NULL,
                         choiceNames = NULL,
                         choiceValues = NULL,
                         selected = NULL,
                         status = "primary",
                         replace = FALSE,
                         badge = TRUE,
                         ncolSource = "auto",
                         ncolGrid = NULL,
                         dragulaOpts = list(),
                         boxStyle = NULL,
                         width = NULL,
                         height = "100px") {

  args <- normalizeChoicesArgs(choices, choiceNames, choiceValues)

  if (!is.null(selected) && !is.list(selected))
    stop("If provided 'selected' must be a list.", call. = FALSE)

  targets <- generate_targets(inputId, args, targetsLabels, targetsIds, selected, replace, boxStyle, badge, status, height)
  targetsIds <- targets$ids
  replaceTargets <- targets$replace

  if (is.null(ncolGrid))
    ncolGrid <- length(targetsIds)

  if (identical(ncolSource, "auto"))
    ncolSource <- ncolGrid

  if (!isTRUE(replace) & !is.null(selected)) {
    ind <- unlist(args$choiceValues, use.names = FALSE) %in% unlist(selected, use.names = FALSE)
    args$choiceValues <- args$choiceValues[!ind]
    args$choiceNames <- args$choiceNames[!ind]
  }

  tagList(
    html_dependency_dragula(),
    tags$label(
      label,
      `for` = inputId,
      class = "control-label",
      class = if (is.null(label)) "shiny-label-null"
    ),
    tags$div(
      class="form-group shiny-input-container shiny-input-dragula shiny-input-container-inline",
      style = if (!is.null(width)) paste("width:", validateCssUnit(width), ";"),
      # style = if (!is.null(height)) paste("height:", validateCssUnit(height), ";"),
      id = inputId,
      tags$div(
        style = "display: -ms-grid; display: grid;",
        style = "grid-column-gap: 5px; grid-row-gap: 5px;",
        style = sprintf("grid-template-columns: repeat(%s, 1fr);", ncolGrid),
        style = sprintf("-ms-grid-columns: %s;", rep("1fr", ncolGrid)),
        tags$div(
          class = "container-drag-source",
          style = if (!is.null(ncolSource)) paste0("grid-area: 1 / 1 / auto / span ", ncolSource, ";"),
          style = boxStyle,
          style = make_bg_svg(sourceLabel),
          style = if (!is.null(height)) paste("height:", validateCssUnit(height), ";"),
          tags$div(
            id = paste(inputId, "source", sep = "-"),
            style = "min-height: 15px;",
            makeDragulaChoices(inputId = inputId, args = args, status = status, badge = badge)
          )
        ),
        targets$targets
      ),
      tags$script(
        type = "application/json",
        `data-for` = inputId,
        toJSON(list(
          source = list1(paste(inputId, "source", sep = "-")),
          targets = list1(paste(inputId, "target", targetsIds, sep = "-")),
          replace = replace,
          replaceIds = list1(replaceTargets),
          options = dragulaOpts
        ), auto_unbox = TRUE, json_verbatim = TRUE)
      )
    )
  )
}


generate_targets <- function(inputId, args, targetsLabels, targetsIds, selected, replace, boxStyle, badge, status, height) {
  if (is.null(targetsIds)) {
    targetsIds <- makeId(targetsLabels)
  } else {
    stopifnot(length(targetsLabels) == length(targetsIds))
    targetsIds <- makeId(targetsIds)
  }

  replaceTargets <- targetsIds
  if (is.numeric(replace)) {
    replaceTargets <- targetsIds[replace]
    replace <- TRUE
  } else {
    stopifnot(is.logical(replace))
  }
  replaceTargets <- paste0(inputId, "-target-", replaceTargets)

  targets <- lapply(
    X = seq_along(targetsLabels),
    FUN = function(i) {
      if (is.null(selected)) {
        tags$div(
          style = "margin: 0;",
          style = if (!is.null(height)) paste("height:", validateCssUnit(height), ";"),
          style = boxStyle,
          class = "box-dad xyvar dragula-target",
          id = paste(inputId, "target", targetsIds[i], sep = "-"),
          style = make_bg_svg(targetsLabels[i])
        )
      } else {
        choicesTarget <- get_choices(args, selected[[targetsIds[i]]])
        tags$div(
          style = "margin: 0;",
          style = if (!is.null(height)) paste("height:", validateCssUnit(height), ";"),
          style = boxStyle,
          class = "box-dad xyvar dragula-target",
          id = paste(inputId, "target", targetsIds[i], sep = "-"),
          style = make_bg_svg(targetsLabels[i]),
          makeDragulaChoices(inputId = inputId, args = choicesTarget, status = status, badge = badge)
        )
      }
    }
  )
  list(
    ids = targetsIds,
    replace = replaceTargets,
    targets = targets
  )
}


#' @importFrom jsonlite base64_enc
#' @importFrom htmltools doRenderTags tag
make_bg_svg <- function(text) {
  svg <- tag("svg", list(
    xmlns = "http://www.w3.org/2000/svg",
    version = "1.1",
    tag("text", list(
      x = "100%",
      y = "20",
      opacity = "0.15",
      fill = "E6E6E6",
      "font-weight" = "bold",
      "font-family" = "Helvetica, Arial, sans-serif",
      "font-size" = "20",
      "text-anchor" = "end",
      transform = "translate(-2,0)",
      text
    ))
  ))
  svg <- doRenderTags(svg)
  svg <- base64_enc(svg)
  svg <- gsub(x = svg, pattern = "\n", replacement = "")
  svg <- sprintf(
    "background-image:url(\"data:image/svg+xml;base64,%s\");",
    svg
  )
  paste0(svg, "background-color:white; background-repeat:no-repeat; background-position:right bottom;")
}


#' Update Dragula Input
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param inputId The id of the input object.
#' @param choices List of values to select from (if elements of the list are
#'  named then that name rather than the value is displayed to the user).
#'  If this argument is provided, then \code{choiceNames} and \code{choiceValues} must
#'  not be provided, and vice-versa. The values should be strings; other
#'  types (such as logicals and numbers) will be coerced to strings.
#' @param choiceNames,choiceValues List of names and values, respectively,
#' that are displayed to the user in the app and correspond to the each
#' choice (for this reason, choiceNames and choiceValues must have the same length).
#' If either of these arguments is provided, then the other must be provided and
#' choices must not be provided. The advantage of using both of these over a named
#' list for choices is that choiceNames allows any type of UI object to be passed
#' through (tag objects, icons, HTML code, ...), instead of just simple text.
#' @param selected A \code{list} with \code{targetIds} as names to select values.
#' @param selectedNames,selectedValues Update selected items with custom names and values.
#' @param badge Displays choices inside a Bootstrap badge.
#' @param status If choices are displayed into a Bootstrap badge, you can use Bootstrap
#'  status to color them, or \code{NULL}.
#'
#'
#' @export
#'
#' @importFrom htmltools doRenderTags
#'
#' @examples
#'
#' if (interactive()) {
#'
#' library("shiny")
#' library("esquisse")
#'
#' ui <- fluidPage(
#'   tags$h2("Update dragulaInput"),
#'   radioButtons(
#'     inputId = "update",
#'     label = "Dataset",
#'     choices = c("iris", "mtcars")
#'   ),
#'   tags$br(),
#'   dragulaInput(
#'     inputId = "myDad",
#'     sourceLabel = "Variables",
#'     targetsLabels = c("X", "Y", "fill", "color", "size"),
#'     choices = names(iris),
#'     replace = TRUE, width = "400px", status = "success"
#'   ),
#'   verbatimTextOutput(outputId = "result")
#' )
#'
#' server <- function(input, output, session) {
#'
#'   output$result <- renderPrint(str(input$myDad))
#'
#'   observeEvent(input$update, {
#'     if (input$update == "iris") {
#'       updateDragulaInput(
#'         session = session,
#'         inputId = "myDad",
#'         choices = names(iris),
#'         status = "success"
#'       )
#'     } else {
#'       updateDragulaInput(
#'         session = session,
#'         inputId = "myDad",
#'         choices = names(mtcars)
#'       )
#'     }
#'   }, ignoreInit = TRUE)
#'
#' }
#'
#' shinyApp(ui, server)
#'
#' }
#'
updateDragulaInput <- function(session,
                               inputId,
                               choices = NULL,
                               choiceNames = NULL,
                               choiceValues = NULL,
                               selected = NULL,
                               selectedNames = NULL,
                               selectedValues = NULL,
                               badge = TRUE,
                               status = "primary") {
  if (!is.null(choices) | !is.null(choiceNames)) {
    args <- normalizeChoicesArgs(choices, choiceNames, choiceValues)
    choices <- doRenderTags(makeDragulaChoices(
      inputId = session$ns(inputId),
      args = args,
      status = status,
      badge = badge
    ))
  }
  if (!is.null(selected) && length(selected) > 0) {
    nms <- names(selected)
    selected <- lapply(
      X = selected,
      FUN = function(x) {
        choicesTarget <- normalizeChoicesArgs(x, NULL, NULL)
        doRenderTags(makeDragulaChoices(
          inputId = session$ns(inputId),
          args = choicesTarget,
          status = status,
          badge = badge
        ))
      }
    )
    names(selected) <- paste(session$ns(inputId), "target", nms, sep = "-")
  }
  if (!is.null(selectedNames) & !is.null(selectedValues)) {
    if (length(selectedNames) != length(selectedValues))
      stop("'selectedValues' & 'selectedNames' must be of same length", call. = FALSE)
    nms <- names(selectedNames)
    selected <- lapply(
      X = seq_along(selectedNames),
      FUN = function(i) {
        choicesTarget <- normalizeChoicesArgs(NULL, selectedNames[[i]], selectedValues[[i]])
        doRenderTags(makeDragulaChoices(
          inputId = session$ns(inputId),
          args = choicesTarget,
          status = status,
          badge = badge
        ))
      }
    )
    names(selected) <- paste(session$ns(inputId), "target", nms, sep = "-")
  }
  message <- dropNulls(list(choices = choices, selected = selected))
  session$sendInputMessage(inputId, message)
}



makeDragulaChoices <- function(inputId, args, status = NULL, badge = TRUE) {
  lapply(
    X = seq_along(args$choiceNames),
    FUN = function(i) {
      tags$div(
        class = "dragula-block",
        tags$span(
          class = "label-dragula",
          class = if (badge) "label badge-dragula",
          class = if (badge & !is.null(status)) paste0("label-", status),
          id = paste(inputId, "target-label", makeId(args$choiceValues[[i]]), sep = "-"),
          `data-value` = args$choiceValues[[i]],
          args$choiceNames[[i]]
        )
      )
    }
  )
}

get_choices <- function(x, values) {
  if (is.null(values))
    return(NULL)
  ind <- vapply(x$choiceValues, function(x) {
    x %in% values
  }, FUN.VALUE = logical(1))
  list(
    choiceNames = x$choiceNames[ind],
    choiceValues = x$choiceValues[ind]
  )
}



#' @importFrom htmltools htmlDependency
html_dependency_dragula <- function() {
  htmlDependency(
    name = "esquisse-dragula",
    version = packageVersion("esquisse"),
    src = c(file = "assets/dragula", href = "esquisse/dragula"),
    package = "esquisse",
    script = c("dragula.min.js", "dragula-bindings.js"),
    stylesheet = c("dragula.min.css", "styles-dad.css"),
    all_files = FALSE
  )
}




