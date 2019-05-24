
#' Safely render a \code{ggplot} in Shiny application
#'
#' @param expr Code to produce a \code{ggplot} object.
#' @param data Argument passed to \code{\link[rlang]{eval_tidy}} to evaluate expression.
#' @param session Session object to send notification to.
#'
#' @return Output of \code{\link[ggplot2]{ggplot_build}}.
#' @export
#' 
#' @importFrom shiny showNotification getDefaultReactiveDomain
#' @importFrom tools toTitleCase
#' @importFrom rlang eval_tidy
#' @importFrom ggplot2 ggplot_build
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(ggplot2)
#'   
#'   ui <- fluidPage(
#'     fluidRow(
#'       column(
#'         width = 3,
#'         selectInput(
#'           inputId = "var", 
#'           label = "Var:", 
#'           choices = c("Sepal.Width", "Do.Not.Exist")
#'         )
#'       ),
#'       column(
#'         width = 9,
#'         plotOutput(outputId = "plot")
#'       )
#'     )
#'   )
#'   
#'   server <- function(input, output, session) {
#'     
#'     output$plot <- renderPlot({
#'       p <- ggplot(iris) +
#'         geom_point(aes_string("Sepal.Length", input$var))
#'       safe_ggplot(p)
#'     })
#'     
#'   }
#'   
#'   shinyApp(ui, server)
#' }
safe_ggplot <- function(expr, data = NULL, session = shiny::getDefaultReactiveDomain()) {
  show_condition_message <- function(e, type, session) {
    if (!is.null(session)) {
      shiny::showNotification(
        ui = paste(
          tools::toTitleCase(type),
          conditionMessage(e),
          sep = " : "
        ),
        type = type, 
        session = session
      )
    }
  }
  withCallingHandlers(
    expr = tryCatch(
      expr = {
        gg <- eval_tidy(expr = expr, data = data)
        gg <- ggplot_build(gg)
        gg
      },
      error = function(e) {
        show_condition_message(e, "error", session)
        list(plot = NULL, data = NULL, layout = NULL)
      }
    ), 
    warning = function(w) {
      show_condition_message(w, "warning", session)
      list(plot = NULL, data = NULL, layout = NULL)
    }
  )
}
