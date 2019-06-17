

#' Drag And Drop Input Widget
#'
#' @param inputId The \code{input} slot that will be used to access the value.
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
#' @param badge Displays choices inside a Bootstrap badge. Use \code{FALSE}
#'  if you want to pass custom appearance with \code{choiceNames}.
#' @param status If choices are displayed into a Bootstrap label, you can use Bootstrap
#'  status to color them, or \code{NULL}.
#' @param replace When a choice is dragged in a target container already
#'  containing a choice, does the later be replaced by the new one ?
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
#' @examples
#' 
#' if (interactive()) {
#' 
#' library("shiny")
#' library("esquisse")
#' 
#' ui <- fluidPage(
#'   tags$h2("Demo dragulaInput"),
#'   tags$br(),
#'   dragulaInput(
#'     inputId = "dad",
#'     sourceLabel = "Source",
#'     targetsLabels = c("Target 1", "Target 2"),
#'     choices = names(iris),
#'     width = "400px"
#'   ),
#'   verbatimTextOutput(outputId = "result")
#' )
#' 
#' 
#' server <- function(input, output, session) {
#'   
#'   output$result <- renderPrint(str(input$dad))
#' 
#' }
#' 
#' shinyApp(ui = ui, server = server)
#' 
#' }
#' 
dragulaInput <- function(inputId, sourceLabel, targetsLabels, 
                         targetsIds = NULL,
                         choices = NULL, choiceNames = NULL,
                         choiceValues = NULL, status = "primary", 
                         replace = FALSE, badge = TRUE, width = NULL, height = "200px") {
  
  args <- normalizeChoicesArgs(choices, choiceNames, choiceValues)
  
  if (is.null(targetsIds)) {
    targetsIds <- gsub(pattern = "[^[:alnum:]]", replacement = "", x = targetsLabels)
  } else {
    stopifnot(length(targetsLabels) == length(targetsIds))
  }
  
  replace_targets <- targetsIds
  if (is.numeric(replace)) {
    replace_targets <- targetsIds[replace]
    replace <- TRUE
  } else {
    stopifnot(is.logical(replace))
  }
  replace_targets <- paste0("dragvars-target-", replace_targets)
  
  target_list <- lapply(
    X = seq_along(targetsLabels),
    FUN = function(i) {
      tags$div(
        style = "height: 95%; margin: 0;",
        class = "box-dad xyvar", id = paste(inputId, "target", targetsIds[i], sep = "-"),
        # tags$em(tags$b(targetsLabels[i], class = "label-background"))
        style = make_bg_svg(targetsLabels[i])
      )
    }
  )
  target_list$style <- "height: 50%; font-size: 0;"
  target_list$cellArgs <- list(style = "height:90%; padding: 0; margin-right: 0.5%;")
  target_list$width <- width
  
  tgw <- 100 / length(targetsIds)
  tgw <- tgw - 0.5 # / (length(targetsIds)-1)
  tgw <- paste0(tgw, "%")
  target_list$cellWidths <- tgw
  
  tagList(
    singleton(
      tags$head(
        tags$script(src = "esquisse/dragula/dragula.min.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "esquisse/dragula/dragula.min.css"),
        tags$script(src = "esquisse/dragula/dragula-bindings.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "esquisse/styles-dad.css")
      )
    ),
    tags$div(
      class="form-group shiny-input-container shiny-input-dragula shiny-input-container-inline",
      style = if(!is.null(width)) paste("width:", htmltools::validateCssUnit(width), ";"),
      style = if(!is.null(height)) paste("height:", htmltools::validateCssUnit(height), ";"),
      id = inputId, #style = "height: 200px;", 
      `data-source` = jsonlite::toJSON(paste(inputId, "source", sep = "-")),
      `data-targets` = jsonlite::toJSON(paste(inputId, "target", targetsIds, sep = "-")),
      `data-replace` = tolower(replace),
      `data-replace-ids` = jsonlite::toJSON(x = replace_targets),
      tags$div(
        style = "height: 50%; width: 99.5%; padding-right: 0; padding-left: 0; margin-right: 0; margin-left: 0;",
        class = "box-dad",
        style = make_bg_svg(sourceLabel),
        # tags$em(tags$b(sourceLabel, class = "label-background")),
        tags$div(
          id = paste(inputId, "source", sep = "-"), 
          style = "margin: 5px; width: 100%; min-height: 15px; margin-right: 0;",
          makeDragulaChoices(inputId = inputId, args = args, status = status, badge = badge)
        )
      ),
      do.call(splitLayout, target_list)
    )
  )
}


#' @importFrom jsonlite base64_enc
#' @importFrom stringi stri_replace_all_fixed
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
      "font-size" = "24",
      "text-anchor" = "end",
      text
    ))
  ))
  svg <- doRenderTags(svg)
  svg <- base64_enc(svg)
  svg <- stri_replace_all_fixed(svg, pattern = "\n", replacement = "")
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
updateDragulaInput <- function(session, inputId, choices = NULL, choiceNames = NULL,
                               choiceValues = NULL, badge = TRUE, status = "primary") {
  args <- normalizeChoicesArgs(choices, choiceNames, choiceValues)
  choices <- htmltools::doRenderTags(makeDragulaChoices(
    inputId = inputId, args = args, status = status, badge = badge
  ))
  message <- list(choices = choices)
  session$sendInputMessage(inputId, message)
}



makeDragulaChoices <- function(inputId, args, status = NULL, badge = TRUE) {
  lapply(
    X = seq_along(args$choiceNames),
    FUN = function(i) {
      tags$span(
        class = "label-dragula",
        class = if (badge) "label", 
        class = if (badge & !is.null(status)) paste0("label-", status), 
        # id = paste(inputId, "target-label", sep = "-"),
        id = paste(inputId, "target-label", clean_string(args$choiceValues[[i]]), sep = "-"),
        `data-value` = args$choiceValues[[i]],
        args$choiceNames[[i]]
      )
    }
  )
}




normalizeChoicesArgs <- function (choices, choiceNames, choiceValues, mustExist = TRUE) {
  if (is.null(choices)) {
    if (is.null(choiceNames) || is.null(choiceValues)) {
      if (mustExist) {
        stop("Please specify a non-empty vector for `choices` (or, ",
             "alternatively, for both `choiceNames` AND `choiceValues`).")
      }
      else {
        if (is.null(choiceNames) && is.null(choiceValues)) {
          return(list(choiceNames = NULL, choiceValues = NULL))
        }
        else {
          stop("One of `choiceNames` or `choiceValues` was set to ",
               "NULL, but either both or none should be NULL.")
        }
      }
    }
    if (length(choiceNames) != length(choiceValues)) {
      stop("`choiceNames` and `choiceValues` must have the same length.")
    }
    if (anyNamed(choiceNames) || anyNamed(choiceValues)) {
      stop("`choiceNames` and `choiceValues` must not be named.")
    }
  }
  else {
    if (!is.null(choiceNames) || !is.null(choiceValues)) {
      warning("Using `choices` argument; ignoring `choiceNames` and `choiceValues`.")
    }
    choices <- choicesWithNames(choices)
    choiceNames <- names(choices)
    choiceValues <- unname(choices)
  }
  return(list(choiceNames = as.list(choiceNames), choiceValues = as.list(as.character(choiceValues))))
}


choicesWithNames <- function (choices)
{
  listify <- function(obj) {
    makeNamed <- function(x) {
      if (is.null(names(x)))
        names(x) <- character(length(x))
      x
    }
    res <- lapply(obj, function(val) {
      if (is.list(val))
        listify(val)
      else if (length(val) == 1 && is.null(names(val)))
        val
      else makeNamed(as.list(val))
    })
    makeNamed(res)
  }
  choices <- listify(choices)
  if (length(choices) == 0)
    return(choices)
  choices <- mapply(choices, names(choices), FUN = function(choice,
                                                            name) {
    if (!is.list(choice))
      return(choice)
    if (name == "")
      stop("All sub-lists in \"choices\" must be named.")
    choicesWithNames(choice)
  }, SIMPLIFY = FALSE)
  missing <- names(choices) == ""
  names(choices)[missing] <- as.character(choices)[missing]
  choices
}


anyNamed <- function (x) 
{
  if (length(x) == 0) 
    return(FALSE)
  nms <- names(x)
  if (is.null(nms)) 
    return(FALSE)
  any(nzchar(nms))
}

