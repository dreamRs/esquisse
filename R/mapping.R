
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
#' @example examples/build_aes.R
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


combine_aes <- function(...) {
  mapping <- c(...)
  mapping <- mapping[!duplicated(names(mapping), fromLast = TRUE)]
  mapping <- aes(!!!syms2(mapping))
  mapping[!duplicated(names(mapping), fromLast = TRUE)]
}

