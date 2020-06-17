
#' Build aesthetics to use in a plot
#'
#' @param data Data to use in the plot.
#' @param ... Named list of aesthetics.
#' @param .list Alternative to \code{...} to use a preexisting named list.
#' @param geom Geom to use, according to the geom aesthetics may vary.
#'
#' @return An expression
#' @export
#' 
#' @importFrom rlang syms expr
#' @importFrom ggplot2 aes
#'
#' @examples
#' # Classic
#' build_aes(iris, x = "Sepal.Width")
#' build_aes(iris, x = "Sepal.Width", y = "Sepal.Width")
#' 
#' # Explicit geom : no change
#' build_aes(iris, x = "Species", geom = "bar")
#' 
#' # Little trick if data is count data
#' df <- data.frame(
#'   LET = c("A", "B"),
#'   VAL = c(4, 7)
#' )
#' build_aes(df, x = "LET", y = "VAL", geom = "bar")
#' 
#' # e.g. :
#' library(ggplot2)
#' ggplot(df) + 
#'   build_aes(df, x = "LET", y = "VAL", geom = "bar") + 
#'   geom_bar()
build_aes <- function(data, ..., .list = NULL, geom = NULL) {
  if (is.null(data))
    return(aes())
  args <- c(list(...), .list)
  args <- dropNulls(args)
  
  if (is.null(geom))
    geom <- "auto"
  args <- syms(args)
  data_mapped <- lapply(args, rlang::eval_tidy, data = data)
  x_type <- col_type(data_mapped$x, no_id = TRUE)
  y_type <- col_type(data_mapped$y, no_id = TRUE)
  if (is.null(args$x) & !is.null(args$y) & geom %in% "line") {
    args$x <- expr(seq_along(!!args$y))
  }
  if (!is.null(args$x) & is.null(args$y) & geom %in% c("boxplot", "violin")) {
    args$y <- args$x
    args$x <- I("")
  }
  if (is.null(args$x) & !is.null(args$y) & geom %nin% c("boxplot", "violin")) {
    tmp <- args$y
    args$y <- args$x
    args$x <- tmp
  }
  if (!is.null(args$x) & !is.null(args$y) & geom %in% c("boxplot", "violin")) {
    if (x_type == "continuous" & (!is.null(y_type) && y_type == "discrete")) {
      tmp <- args$y
      args$y <- args$x
      args$x <- tmp
    }
  }
  if (!is.null(args$x) & !is.null(args$y) & geom == "bar") {
    if (x_type == "continuous" & y_type == "discrete") {
      args$weight <- args$x
      args$x <- args$y
      args$y <- NULL
    }
    if (x_type %in% c("discrete", "time") & y_type == "continuous") {
      args$weight <- args$y
      args$y <- NULL
    }
  }
  eval(expr(aes(!!!args)))
}




make_aes <- function(.list) {
  .list$facet <- NULL
  .list$facet_row <- NULL
  .list$facet_col <- NULL
  if (!is.null(.list$xvar)) {
    .list$x <- .list$xvar
    .list$xvar <- NULL
  }
  if (!is.null(.list$yvar)) {
    .list$y <- .list$yvar
    .list$yvar <- NULL
  }
  .list
}




