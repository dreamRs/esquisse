

#' Dropdown input with images
#'
#' @param id Module's id
#' @param imgs a list of parameters for choices, possible fields are :
#' \code{inputId}, \code{img} (path to the image), \code{label} optional label.
#' @param selected Default selected value.
#' @param up Logical. Display the dropdown menu above.
#' @param width Width of the dropdown menu content.
#'
#' @return A ui definition
#' @noRd
#'
#' @importFrom htmltools validateCssUnit tags tagList
#' @importFrom shiny actionButton NS icon
#'
imageButtonUI <- function(id, imgs = list(), selected = 1, up = FALSE, width = NULL) {

  # Namespace
  ns <- NS(id)

  imgButton <- function(inputId, img, label) {
    actionButton(
      inputId = inputId,
      label = tagList(
        tags$img(src = img, width = 70, height = 70),
        tags$br(), label
      ),
      style = "border: none;"
    )
  }

  btn <- actionButton(
    inputId = ns("btn-action"),
    label = tags$img(
      id = ns("btn-img"),
      src = imgs[[selected]]$img,
      width = 48, height = 48
    ), width = "100%",
    class = "dropdown-toggle", 
    `data-toggle` = "dropdown"
  )
  dropTag <- tags$div(
    class = "dropdown-menu pre-scrollable",
    style = "padding: 5px; max-height: 80vh;",
    style = if (!is.null(width))
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(
      X = imgs,
      FUN = function(x) {
        imgButton(
          inputId = ns(x$inputId),
          label = x$label,
          img = x$img
        )
      }
    )
  )

  tagList(
    toggleInputUi(),
    tags$div(
      class = ifelse(up, "dropup", "dropdown"),
      style = "margin-bottom: 1vh;",
      btn, dropTag
    ),
    tags$script(
      paste0(
        "Shiny.addCustomMessageHandler('", 
        ns("update-img"), "', function(data) {",
        "$('#", ns("btn-img"), "').attr('src', data);",
        "});"
      )
    )
  )
}


#' Dropdown input with images
#'
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param default default value
#' @param img_ref Ids of image to toggle on/off
#' @param enabled Images to enabled
#' @param selected Image to select
#'
#' @return a reactivalues with the value of the inputs
#' @noRd
#'
#' @importFrom htmltools validateCssUnit tags tagList
#' @importFrom shiny observeEvent reactiveValues observe
#'
imageButtonServer <- function(input, output, session, default = NULL, img_ref = list(), enabled = NULL, selected = NULL) {

  # Namespace
  ns <- session$ns

  if (is.null(enabled))
    enabled <- reactiveValues(x = NULL)
  
  if (is.null(selected))
    selected <- reactiveValues(x = NULL)

  observeEvent(enabled$x, {
    if (!is.null(enabled$x)) {
      for (i in names(img_ref)) {
        if (i %in% enabled$x) {
          toggleInputServer(session = session, inputId = ns(i), enable = TRUE)
        } else {
          toggleInputServer(session = session, inputId = ns(i), enable = FALSE)
        }
      }
    }
  })

  r <- reactiveValues(x = default)

  observe({
    lapply(
      X = names(input),
      FUN = function(x) {
        if (x != "btn-action") {
          observeEvent(input[[x]], {
            r$x <- x
            session$sendCustomMessage(ns("update-img"), img_ref[[x]])
          })
        }
      }
    )
  })
  
  observeEvent(selected$x, {
    x <- selected$x
    x <- setdiff(x, "auto")
    if (length(x) > 0) {
      x <- x[1]
      r$x <- x
      session$sendCustomMessage(ns("update-img"), img_ref[[x]])
    }
  }, ignoreNULL = TRUE)

  return(r)
}
