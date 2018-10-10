#' Generate code to produce a ggplot
#'
#' @param data Character. Name of the data used in the plot.
#' @param aes Named list. Mapping for the plot
#' @param geom Character. Geom used
#' @param args_geom Named list. Additional arguments for the geom, like \code{color}, \code{size},...
#' @param scale Character, the scale to use.
#' @param labs Named list, optional. Title, axis labels, subtitle.. for the plot.
#' @param facet Variable to use in \code{facet_wrap}.
#' @param theme Character. Theme used, if any.
#' @param coord Character. Coordinate system to use.
#' @param params List, other parameters (legend.position, ...)
#'
#' @return a character string
#' @noRd
#'
#' @examples
#' ggcode(
#'   data = "mtcars",
#'   aes = list(x = "hp", y = "wt"),
#'   geom = "point"
#' )
#'
#' ggcode(
#'   data = "mtcars",
#'   aes = list(x = "wt"),
#'   geom = "histogram",
#'   args_geom = list(fill = "steelblue", bins = 15)
#' )
ggcode <- function(data, aes, geom, args_geom = NULL, scale = NULL, labs = NULL, facet = NULL, theme = NULL, coord = NULL, params = list()) {
  l2char <- function(l, quote = FALSE, sep_args = ", ") {
    if (quote) {
      l <- lapply(
        X = l,
        FUN = function(x) {
          if (inherits(x, what = c("numeric", "logical", "integer"))) {
            x
          } else {
            shQuote(x)
          }
        }
      )
    }
    paste(paste(names(l), unlist(l), sep = " = "), collapse = sep_args)
  }
  aes <- dropNulls(aes)
  c_ggplot <- sprintf("ggplot(data = %s)", data)
  c_aes <- sprintf("aes(%s)", l2char(aes))
  c_geom <- sprintf("geom_%s(%s)", geom, l2char(args_geom, TRUE))

  labs <- dropNullsOrEmpty(labs)
  if (!is.null(labs) && length(labs) > 0) {
    c_labs <- sprintf("labs(%s)", l2char(labs, TRUE, ",\n    "))
  } else {
    c_labs <- NULL
  }

  if (!is.null(theme)) {
    # c_theme <- paste0("theme_", theme, "()")
    c_theme <- gsub(pattern = ".*::", replacement = "", x = theme)
    c_theme <- paste0(c_theme, "()")
  } else {
    c_theme <- NULL
  }
  if (!is.null(coord)) {
    c_coord <- paste0("coord_", coord, "()")
  } else {
    c_coord <- NULL
  }
  
  if (!is.null(params$smooth_add) && params$smooth_add) {
    c_sm <- sprintf(" +\n  geom_smooth(span = %s)", params$smooth_span)
    c_geom <- paste0(c_geom, c_sm)
  }
  
  if (!is.null(facet)) {
    c_facet <- sprintf("facet_wrap(vars(%s))", paste(facet, collapse = ", "))
  } else {
    c_facet <- NULL
  }

  if (!is.null(aes$fill) | !is.null(aes$color) | !is.null(aes$size)) {
    if (!is.null(params$legend_position) && params$legend_position != "right") {
      c_theme <- paste0(c_theme, " +\n  theme(legend.position = '", params$legend_position, "')")
    }
  }

  c_gg <- c(c_ggplot, c_aes, c_geom, scale, c_labs, c_theme, c_facet, c_coord)
  paste0(paste(c_gg, collapse = " +\n  "), "\n")
}
