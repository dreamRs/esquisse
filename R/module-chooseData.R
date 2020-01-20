
#' @title Module for choosing data.frame
#' 
#' @description Module for choosing data.frame from
#' user environment and select variable to use.
#'
#' @param id Module's id.
#' @param label Button's label.
#' @param icon Button's icon.
#' @param ... Arguments passed to \code{\link{actionButton}}
#'
#' @return a \code{\link[shiny]{reactiveValues}} containing the data selected under slot \code{data}
#' and the name of the selected \code{data.frame} under slot \code{name}.
#' @export
#' 
#' @name module-chooseData
#' 
#' @importFrom htmltools tagList tags singleton
#' @importFrom shiny NS actionButton icon 
#'
#' @example examples/chooseData.R
chooseDataUI <- function(id, label = "Data", icon = "database", ...) {
  
  ns <- NS(id)
  
  if (is.character(icon))
    icon <- icon(icon)

  tagList(
    singleton(
      tags$link(rel="stylesheet", type="text/css",
                href="esquisse/styles-dad.css")
    ),
    useShinyUtils(),
    actionButton(
      inputId = ns("changeData"), label = label,
      icon = icon, width = "100%", ...
    )
  )
}

#' @param input,output,session standards \code{shiny} server arguments.
#' @param dataModule Data module to use, choose between \code{"GlobalEnv"}
#'  (select ad \code{data.frame} from Global environment)
#'  or \code{"ImportFile"} (import an external file supported by \code{\link[rio]{import}}).
#' @param data A \code{data.frame} to use by default.
#' @param name Character, object's name to use for \code{data}.
#' @param selectVars Display module to select variables, \code{TRUE} by default.
#' @param selectedTypes Type of variables selected by default in select variables module.
#'  Possible types are \code{"discrete"}, \code{"time"}, \code{"continuous"} and \code{"id"}, 
#'  by default \code{"id"} is discarded.
#' @param coerceVars Display module to coerce variables between different class, \code{TRUE} by default.
#' @param launchOnStart Opens modal window when the application starts.
#' @param size Size for the modal window.
#' 
#' 
#' @export
#'
#' @rdname module-chooseData
#'
#' @importFrom shiny showModal modalDialog observeEvent reactiveValues callModule observe icon
#' @importFrom htmltools tags HTML
chooseDataServer <- function(input, output, session, 
                             dataModule = c("GlobalEnv", "ImportFile"), 
                             data = NULL, name = NULL, 
                             selectVars = TRUE, 
                             selectedTypes = c("continuous", "discrete", "time"), 
                             coerceVars = FALSE, 
                             launchOnStart = TRUE, size = "m") {
  
  dataModule <- match.arg(dataModule)
  datModUI <- switch(
    dataModule,
    "GlobalEnv" = dataGlobalEnvUI,
    "ImportFile" = dataImportFileUI
  )
  datModServer <- switch(
    dataModule,
    "GlobalEnv" = dataGlobalEnvServer,
    "ImportFile" = dataImportFileServer
  )

  ns <- session$ns
  return_data <- reactiveValues(data = data, name = name)
  
  if (isTRUE(launchOnStart)) {
    showModal(modalDialog(tagList(
      tags$button(
        icon("close"), 
        class = "btn btn-link pull-right",
        `data-dismiss` = "modal"
      ),
      datModUI(
        id = ns("chooseData"), 
        selectVars = selectVars, 
        coerceVars = coerceVars
      )
    ), size = size, fade = FALSE, footer = NULL))
  }
  
  observeEvent(input$changeData, {
    showModal(modalDialog(tagList(
      tags$button(
        icon("close"), 
        class = "btn btn-link pull-right",
        `data-dismiss` = "modal"
      ),
      datModUI(
        id = ns("chooseData"), 
        selectVars = selectVars, 
        coerceVars = coerceVars
      )
    ), size = size, fade = FALSE, footer = NULL))
  })
  
  return_data <- callModule(
    module = datModServer, 
    id = "chooseData", 
    data = data, 
    name = name,
    selectedTypes = selectedTypes
  )
  
  return(return_data)
}


