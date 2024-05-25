
# Show data -------------------------------------------------------------------------

#' @importFrom shiny NS
#' @importFrom htmltools tags css
#' @importFrom phosphoricons ph
show_data_ui <- function(id) {
  ns <- NS(id)
  icon <- tags$div(
    style = css(position = "relative", width = "35px"),
    ph("table", height = "2em", title = i18n("Show data")),
    ph(
      "eye",
      style = css(position = "absolute", top = 0, right = 0),
      height = "1.2em",
      weight = "bold",
      title = i18n("Show data")
    )
  )
  btn_header(i18n("Show data"), icon, class = " px-0")(ns("btn"))
}

#' @importFrom shiny moduleServer observeEvent showNotification reactive
#' @importFrom datamods show_data
show_data_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$btn, {
        data <- data_r()
        if (!is.data.frame(data)) {
          showNotification(
            ui = i18n("No data to display"),
            duration = 700,
            id = paste("esquisse", sample.int(1e6, 1), sep = "-"),
            type = "warning"
          )
        } else {
          show_data(data, title = i18n("Dataset"), type = "modal")
        }
      })

    }
  )
}




# Update vars -----------------------------------------------------------------------

#' @importFrom shiny NS
#' @importFrom htmltools tags css
#' @importFrom phosphoricons ph
update_vars_ui <- function(id) {
  ns <- NS(id)
  icon <- tags$div(
    style = css(position = "relative", width = "35px"),
    ph("table", height = "2em", title = i18n("Update variables")),
    ph(
      "gear",
      style = css(position = "absolute", top = 0, right = 0),
      height = "1.2em",
      weight = "bold",
      title = i18n("Update variables")
    )
  )
  btn_header(i18n("Update variables"), icon, class = " px-0")(ns("btn"))
}

#' @importFrom shiny moduleServer observeEvent modalDialog showModal reactive
#' @importFrom datamods update_variables_ui update_variables_server
update_vars_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$btn, {
        showModal(modalDialog(
          title = tagList(
            i18n("Update & select variables"),
            button_close_modal()
          ),
          datamods::update_variables_ui(ns("mod"), title = NULL),
          easyClose = TRUE,
          size = "l",
          footer = NULL
        ))
      })
      res <- datamods::update_variables_server(
        id = "mod",
        data = data_r
      )
      return(res)
    }
  )
}





# Create column ---------------------------------------------------------------------

#' @importFrom shiny NS
create_col_ui <- function(id) {
  ns <- NS(id)
  btn_header(i18n("Create column"), "columns-plus-right")(ns("btn"))
}

#' @importFrom shiny moduleServer observeEvent modalDialog showModal reactive
#' @importFrom datamods update_variables_ui update_variables_server
create_col_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$btn, datamods::modal_create_column(ns("mod")))
      res <- datamods::create_column_server(
        id = "mod",
        data = data_r
      )
      return(res)
    }
  )
}





# Cut variable ------------------------------------------------------------

#' @importFrom shiny NS
#' @importFrom htmltools tags css
#' @importFrom phosphoricons ph
cut_var_ui <- function(id) {
  ns <- NS(id)
  icon <- tags$div(
    style = css(position = "relative", width = "35px"),
    ph("list-numbers", height = "2em", title = i18n("Cut numeric variable into factor")),
    ph(
      "scissors",
      style = css(position = "absolute", top = 0, right = 0, transform = "scale(-1, 1)"),
      height = "1.2em",
      weight = "bold",
      title = i18n("Cut numeric variable into factor")
    )
  )
  btn_header(i18n("Cut numeric variable into factor"), class = "px-0", icon)(ns("btn"))
}

#' @importFrom shiny moduleServer observeEvent modalDialog showModal reactive
#' @importFrom datamods cut_variable_ui cut_variable_server
cut_var_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$btn, datamods::modal_cut_variable(ns("mod")))
      observeEvent(res(), shiny::removeModal())
      res <- datamods::cut_variable_server(
        id = "mod",
        data = data_r
      )
      return(res)
    }
  )
}




# Update factor -----------------------------------------------------------

#' @importFrom shiny NS
#' @importFrom htmltools tags css
#' @importFrom phosphoricons ph
update_fct_ui <- function(id) {
  ns <- NS(id)
  icon <- tags$div(
    style = css(position = "relative", width = "35px"),
    ph("list-dashes", height = "2em", title = i18n("Update factor")),
    ph(
      "arrows-down-up",
      style = css(position = "absolute", top = 0, right = 0, transform = "scale(-1, 1)"),
      height = "1.2em",
      weight = "bold",
      title = i18n("Update factor")
    )
  )
  btn_header(i18n("Update factor"), class = "px-0", icon)(ns("btn"))
}

#' @importFrom shiny moduleServer observeEvent modalDialog showModal reactive
#' @importFrom datamods cut_variable_ui cut_variable_server
update_fct_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$btn, datamods::modal_update_factor(ns("mod")))
      observeEvent(res(), shiny::removeModal())
      res <- datamods::update_factor_server(
        id = "mod",
        data = data_r
      )
      return(res)
    }
  )
}

