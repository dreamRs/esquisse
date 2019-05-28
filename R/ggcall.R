
#' Generate code to create a `ggplot`
#'
#' @param data Character. Name of the \code{data.frame}.
#' @param mapping List. Named list of aesthetics.
#' @param geom Character. Name of the geom to use (without "geom_").
#' @param geom_args List. Arguments to use in the geom.
#' @param scales Character vector. Scale(s) to use (without "scale_").
#' @param scales_args List. Arguments to use in scale(s),
#'  if \code{scales} is length > 1, must be a named list with \code{scales} names.
#' @param coord Character. Coordinates to use (without "coord_").
#' @param labs List. Named list of labels to use for title, subtitle, x & y axis, legends.
#' @param theme Character. Name of the theme to use (without "theme_").
#' @param theme_args List. Named list for theme arguments.
#' @param facet Character vector. Names of variables to use as facet.
#' @param facet_args List. Named list for facet arguments.
#'
#' @return a call
#' @export
#' 
#' @importFrom stats setNames
#' @importFrom rlang sym syms expr as_name is_call
#' @importFrom ggplot2 ggplot aes theme facet_wrap vars coord_flip labs
#'
#' @examples
#' # Default:
#' ggcall()
#' 
#' # With data and aes
#' ggcall("mtcars", list(x = "mpg", y = "wt"))
#' 
#' # Evaluate the call
#' library(ggplot2)
#' eval(ggcall("mtcars", list(x = "mpg", y = "wt")))
#' 
#' 
#' # With a geom:
#' ggcall(
#'   data = "mtcars", 
#'   mapping = list(x = "mpg", y = "wt"), 
#'   geom = "point"
#' )
#' 
#' # With options
#' ggcall(
#'   data = "mtcars", 
#'   mapping = list(x = "hp", y = "cyl", fill = "color"), 
#'   geom = "bar",
#'   coord = "flip", 
#'   labs = list(title = "My title"), 
#'   theme = "minimal", 
#'   facet = c("gear", "carb"),
#'   theme_args = list(legend.position = "bottom")
#' )
#' 
#' # One scale
#' ggcall(
#'   data = "mtcars",
#'   mapping = list(x = "mpg", y = "wt", color = "qsec"),
#'   geom = "point",
#'   scales = "color_distiller", 
#'   scales_args = list(palette = "Blues")
#' )
#' 
#' # Two scales
#' ggcall(
#'   data = "mtcars",
#'   mapping = list(x = "mpg", y = "wt", color = "qsec", size = "qsec"),
#'   geom = "point",
#'   scales = c("color_distiller", "size_continuous"), 
#'   scales_args = list(
#'     color_distiller = list(palette = "Greens"),
#'     size_continuous = list(range = c(1, 20))
#'   )
#' )
ggcall <- function(data = NULL,
                   mapping = NULL, 
                   geom = NULL, 
                   geom_args = list(),
                   scales = NULL, 
                   scales_args = list(),
                   coord = NULL, 
                   labs = list(), 
                   theme = NULL, 
                   theme_args = list(),
                   facet = NULL,
                   facet_args = list()) {
  if (is.null(data))
    return(expr(ggplot()))
  data <- sym(data)
  if (rlang::is_call(mapping)) 
    mapping <- eval(mapping)
  mapping <- dropNulls(mapping)
  syms2 <- function(x) {
    lapply(
      X = x,
      FUN = function(y) {
        if (inherits(y, "AsIs")) {
          as.character(y)
        } else {
          sym(as_name(y))
        }
      }
    )
  }
  aes <- expr(aes(!!!syms2(mapping)))
  ggcall <- expr(ggplot(!!data) + !!aes)
  if (length(geom) == 1)
    geom_args <- setNames(list(geom_args), geom)
  for (g in geom) {
    g_args <- dropNulls(geom_args[[g]])
    geom <- expr((!!sym(paste0("geom_", g)))(!!!g_args))
    ggcall <- expr(!!ggcall + !!geom)
  }
  if (!is.null(scales)) {
    if (length(scales) == 1 && !isTRUE(grepl(scales, names(scales_args))))
      scales_args <- setNames(list(scales_args), scales)
    for (s in scales) {
      s_args <- dropNulls(scales_args[[s]])
      scales <- expr((!!sym(paste0("scale_", s)))(!!!s_args))
      ggcall <- expr(!!ggcall + !!scales)
    }
  }
  labs <- dropNullsOrEmpty(labs)
  if (length(labs) > 0) {
    labs <- expr(labs(!!!labs))
    ggcall <- expr(!!ggcall + !!labs)
  }
  if (!is.null(coord)) {
    coord <- expr((!!sym(paste0("coord_", coord)))())
    ggcall <- expr(!!ggcall + !!coord)
  }
  if (!is.null(theme)) {
    theme <- expr((!!sym(paste0("theme_", theme)))())
    ggcall <- expr(!!ggcall + !!theme)
  }
  if (!any(c("fill", "color", "size") %in% names(mapping))) {
    theme_args$legend.position <- NULL
  }
  theme_args <- dropNullsOrEmpty(theme_args)
  if (length(theme_args) > 0) {
    theme_args <- expr(theme(!!!theme_args))
    ggcall <- expr(!!ggcall + !!theme_args)
  }
  if (!is.null(facet)) {
    facet_args <- dropNullsOrEmpty(facet_args)
    if (length(facet_args) > 0) {
      facet <- expr(facet_wrap(vars(!!!syms(facet)), !!!facet_args))
      ggcall <- expr(!!ggcall + !!facet)
    } else {
      facet <- expr(facet_wrap(vars(!!!syms(facet))))
      ggcall <- expr(!!ggcall + !!facet)
    }
  }
  ggcall
}




