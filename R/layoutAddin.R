#' Addin layout
#'
#' Two rows (1/5, 4/5), first divided in two columns (1/6, 5/6)
#'
#' @param top_left Element to put in the top left corner (1/6)
#' @param top_right Element to put in the top right corner (5/6)
#' @param main Main element
#'
#' @importFrom shiny fillPage fillCol fillRow
#' @noRd
layoutAddin <- function(top_left, top_right, main) {
  shiny::fillPage(
    shiny::fillCol(
      flex = c(1, 4),
      shiny::fillRow(
        flex = c(1, 6), width = "100%", style = "margin: auto;",
        top_left,
        top_right
      ),
      main
    )
  )
}

