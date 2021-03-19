
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
#' @example examples/safe_ggplot.R
safe_ggplot <- function(expr, data = NULL, session = shiny::getDefaultReactiveDomain()) {
  show_condition_message <- function(e, type, session) {
    if (!is.null(session)) {
      showNotification(
        ui = paste(
          tools::toTitleCase(type),
          conditionMessage(e),
          sep = " : "
        ),
        duration = 1000,
        id = paste("esquisse", sample.int(1e6, 1), sep = "-"),
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
