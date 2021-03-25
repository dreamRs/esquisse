
#' @importFrom htmltools tags tagList
#' @importFrom shiny getDefaultReactiveDomain modalDialog
#' @importFrom shinyWidgets alert prettyCheckboxGroup
modal_settings <- function(aesthetics = NULL, session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns
  modalDialog(
    title = tagList(
      "Esquisse settings",
      tags$button(
        icon("close"),
        class = "btn btn-default pull-right",
        style = "border: 0 none;",
        `data-dismiss` = "modal"
      )
    ),
    tags$label(
      "Select aesthetics to be used to build a graph:",
      `for` = ns("aesthetics"),
      class = "control-label"
    ),
    shinyWidgets::alert(
      icon("info"), "Aesthetic mappings describe how variables in the data are mapped to visual properties (aesthetics) of geoms.",
      status = "info"
    ),
    prettyCheckboxGroup(
      inputId = ns("aesthetics"),
      label = NULL,
      choiceNames = list(
        tagList(tags$b("fill:"), "fill color for shapes"),
        tagList(tags$b("color:"), "color points and lines"),
        tagList(tags$b("size:"), "size of the points"),
        tagList(tags$b("shape:"), "shape of the points"),
        tagList(tags$b("weight:"), "frequency weights"),
        tagList(tags$b("group:"), "identifies series of points with a grouping variable"),
        tagList(tags$b("ymin:"), "used in ribbons charts with ymax to display an interval between two lines"),
        tagList(tags$b("ymax:"), "used in ribbons charts with ymin to display an interval between two lines"),
        tagList(tags$b("facet:"), "create small multiples"),
        tagList(tags$b("facet row:"), "create small multiples by rows"),
        tagList(tags$b("facet col:"), "create small multiples by columns")
      ),
      choiceValues = c("fill", "color", "size", "shape", "weight", "group", "ymin", "ymax", "facet", "facet_row", "facet_col"),
      selected = aesthetics %||% c("fill", "color", "size", "facet"),
      status = "primary"
    ),
    easyClose = TRUE,
    footer = NULL
  )
}


