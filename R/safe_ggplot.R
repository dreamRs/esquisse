
#' Safely render a \code{ggplot} in Shiny application
#'
#' @param expr Code to produce a \code{ggplot} object.
#' @param data Argument passed to \code{\link[rlang]{eval_tidy}} to evaluate expression.
#' @param show_notification Strategy for notifications when a warning occurs:
#'   * `"always"` : default, show notifications for each warnings
#'   * `"once` : show notification once per warning
#'   * `"never"` : do not display notifications.
#' @param session Session object to send notification to.
#'
#' @return Output of \code{\link[ggplot2]{ggplot_build}}.
#' @export
#'
#' @importFrom shiny showNotification getDefaultReactiveDomain
#' @importFrom tools toTitleCase
#' @importFrom rlang eval_tidy
#' @importFrom ggplot2 ggplot_build ggplot_gtable
#'
#' @example examples/safe_ggplot.R
safe_ggplot <- function(expr,
                        data = NULL,
                        show_notification = c("always", "once", "never"),
                        session = shiny::getDefaultReactiveDomain()) {
  show_notification <- match.arg(show_notification)
  show_condition_message <- function(e, type, session) {
    if (identical(show_notification, "never")) return(NULL)
    if (!is.null(session)) {
      msg <- conditionMessage(e)
      msg <- gsub("\033[38;5;232m", "", msg, fixed = TRUE)
      msg <- gsub("\033[39m", "", msg, fixed = TRUE)
      msg <- gsub("\033[36m", "", msg, fixed = TRUE)
      msg <- gsub("\033[1m", "", msg, fixed = TRUE)
      msg <- gsub("\033[22m", "", msg, fixed = TRUE)
      msg <- gsub("\033[33m!", "", msg, fixed = TRUE)
      if (identical(show_notification, "once") && isTRUE(msg %in% session$userData$esquisse_notifications)) {
        return(NULL)
      }
      session$userData$esquisse_notifications <- c(
        session$userData$esquisse_notifications,
        msg
      )
      text <- substr(
        htmltools::doRenderTags(tagList(
          tags$b(tools::toTitleCase(type), ":"), msg
        )),
        start = 1, stop = 200
      )
      shinybusy::notify(
        position = "right-bottom",
        text = text,
        timeout = 3000,
        # closeButton = TRUE,
        showOnlyTheLastOne = TRUE,
        plainText = FALSE,
        messageMaxLength = nchar(text),
        clickToClose = TRUE,
        distance = "5px",
        type = type,
        ID = "shiny-notification-esquisse",
        className = "shiny-notification-esquisse",
        session = session
      )
    }
  }
  suppressWarnings(withCallingHandlers(
    expr = tryCatch(
      expr = {
        gg <- eval_tidy(expr = expr, data = data)
        gb <- ggplot_build(gg)
        ggt <- ggplot_gtable(gb)
        return(gb)
      },
      error = function(e) {
        show_condition_message(e, "failure", session)
        list(plot = NULL, data = NULL, layout = NULL)
      }
    ),
    warning = function(w) {
      show_condition_message(w, "warning", session)
    }
  ))
}
