
#' @importFrom htmltools tags tagList
#' @importFrom shiny getDefaultReactiveDomain modalDialog
#' @importFrom shinyWidgets alert prettyCheckboxGroup
modal_settings <- function(aesthetics = NULL, session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns
  modalDialog(
    title = tagList(
      i18n("Esquisse settings"),
      tags$button(
        ph("x"),
        title = i18n("Close"),
        class = "btn btn-default pull-right",
        style = "border: 0 none;",
        `data-dismiss` = "modal"
      )
    ),
    tags$label(
      i18n("Select aesthetics to be used to build a graph:"),
      `for` = ns("aesthetics"),
      class = "control-label"
    ),
    shinyWidgets::alert(
      ph("info"), 
      i18n("Aesthetic mappings describe how variables in the data are mapped to visual properties (aesthetics) of geoms."),
      status = "info"
    ),
    prettyCheckboxGroup(
      inputId = ns("aesthetics"),
      label = NULL,
      choiceNames = list(
        tagList(tags$b("fill:"), i18n("fill color for shapes")),
        tagList(tags$b("color:"), i18n("color points and lines")),
        tagList(tags$b("size:"), i18n("size of the points")),
        tagList(tags$b("shape:"), i18n("shape of the points")),
        tagList(tags$b("weight:"), i18n("frequency weights")),
        tagList(tags$b("group:"), i18n("identifies series of points with a grouping variable")),
        tagList(tags$b("ymin:"), i18n("used in ribbons charts with ymax to display an interval between two lines")),
        tagList(tags$b("ymax:"), i18n("used in ribbons charts with ymin to display an interval between two lines")),
        tagList(tags$b("facet:"), i18n("create small multiples")),
        tagList(tags$b("facet row:"), i18n("create small multiples by rows")),
        tagList(tags$b("facet col:"), i18n("create small multiples by columns"))
      ),
      choiceValues = c("fill", "color", "size", "shape", "weight", "group", "ymin", "ymax", "facet", "facet_row", "facet_col"),
      selected = aesthetics %||% c("fill", "color", "size", "facet"),
      status = "primary"
    ),
    easyClose = TRUE,
    footer = NULL
  )
}


