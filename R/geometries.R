
#' Potential geometries according to the data
#'
#' @param data A \code{data.frame}
#' @param mapping List of aesthetic mappings to use with data.
#' @param auto Return only one geometry.
#'
#' @return A \code{character} vector
#' @export
#'
#' @examples
#' 
#' library(ggplot2)
#' 
#' # One continuous variable
#' potential_geoms(
#'   data = iris,
#'   mapping = aes(x = Sepal.Length)
#' )
#' 
#' # Automatic pick a geom
#' potential_geoms(
#'   data = iris,
#'   mapping = aes(x = Sepal.Length),
#'   auto = TRUE
#' )
#' 
#' # One discrete variable
#' potential_geoms(
#'   data = iris,
#'   mapping = aes(x = Species)
#' )
#' 
#' # Two continuous variables
#' potential_geoms(
#'   data = iris,
#'   mapping = aes(x = Sepal.Length, y = Sepal.Width)
#' )
potential_geoms <- function(data, mapping, auto = FALSE) {
  
  data_mapped <- lapply(mapping, rlang::eval_tidy, data = data)
  
  x_type <- col_type(data_mapped$x, no_id = TRUE)
  y_type <- col_type(data_mapped$y, no_id = TRUE)
  
  potential_geoms_ref <- potential_geoms_ref()
  
  if (is.null(x_type) & !is.null(y_type)) {
    if (all(y_type %nin% "continuous")) {
      x_type <- y_type
      y_type <- "empty"
    } else {
      x_type <- "empty"
    }
  }
  if (!is.null(x_type) & is.null(y_type)) {
    y_type <- "empty"
  }
  
  if (isTRUE(auto)) {
    auto_ <- 1
  } else {
    auto_ <- c(0, 1)
  }
  
  geoms_ind <- eval_tidy(
    expr = expr(!!sym("x") == !!x_type & !!sym("y") %in% !!y_type & auto %in% !!auto_),
    data = potential_geoms_ref
  )
  geoms <- potential_geoms_ref[geoms_ind, "geom"]
  
  if (inherits(data, what = "sf")) {
    if (isTRUE(auto)) {
      geoms <- "sf"
    } else {
      geoms <- c(geoms, "sf")
    }
  }
  
  return(geoms)
}





#' @importFrom ggplot2 geom_histogram geom_density geom_bar geom_sf 
#' geom_boxplot geom_bar geom_point geom_line geom_tile geom_violin
#' geom_area geom_smooth geom_col
potential_geoms_ref <- function() {
  x <- matrix(
    data = c(
      "continuous",  "empty",       "histogram", "1", 
      "continuous",  "empty",       "boxplot",   "0", 
      "continuous",  "empty",       "violin",    "0", 
      "continuous",  "empty",       "density",   "0", 
      "discrete",    "empty",       "bar",       "1", 
      "time",        "empty",       "histogram", "1",
      "time",        "empty",       "bar",       "0",
      "continuous",  "discrete",    "boxplot",   "0", 
      "continuous",  "discrete",    "violin",    "0", 
      "continuous",  "discrete",    "bar",       "1",
      "discrete",    "continuous",  "boxplot",   "0", 
      "discrete",    "continuous",  "violin",    "0", 
      "discrete",    "continuous",  "bar",       "1",
      "continuous",  "continuous",  "point",     "1",
      "continuous",  "continuous",  "line",      "0", 
      "continuous",  "continuous",  "step",      "0", 
      "continuous",  "continuous",  "area",      "0",
      "discrete",    "discrete",    "tile",      "1",
      "time",        "continuous",  "line",      "1",
      "time",        "continuous",  "point",     "0",
      "time",        "continuous",  "step",      "0", 
      "time",        "continuous",  "area",      "0", 
      "time",        "continuous",  "bar",       "0", 
      "empty",       "continuous",  "line",      "1",
      "empty",       "continuous",  "step",      "0",
      "empty",       "continuous",  "area",      "0",
      "continuous",  "continuous",  "tile",      "0",
      "discrete",    "time",        "tile",      "0",
      "time",        "discrete",    "tile",      "0"
    ), ncol = 4, byrow = TRUE
  )
  x <- data.frame(x, stringsAsFactors = FALSE)
  names(x) <- c("x", "y", "geom", "auto")
  x$auto <- as.numeric(x$auto)
  return(x)
}





#' Match list of arguments to arguments of geometry
#'
#' @param geom Character. name of the geometry.
#' @param args Named list, parameters to be matched to the geometry arguments.
#' @param add_aes Add aesthetics parameters (like size, fill, ...).
#' @param mapping Mapping used in plot, to avoid setting fixed aesthetics parameters.
#' @param envir Package environment to search in.
#'
#' @return a \code{list}
#' @export
#'
#' @examples
#' # List of parameters
#' params <- list(
#'   bins = 30,
#'   scale = "width",
#'   adjust = 2,
#'   position = "stack",
#'   size = 1.6,
#'   fill = "#112246"
#' )
#' 
#' # Search arguments according to geom
#' match_geom_args(geom = "histogram", args = params)
#' match_geom_args(geom = "violin", args = params)
#' match_geom_args(geom = "bar", args = params, add_aes = FALSE)
#' match_geom_args(geom = "point", args = params)
#' match_geom_args(geom = "point", args = params, add_aes = FALSE)
match_geom_args <- function(geom, args, add_aes = TRUE, mapping = list(), envir = "ggplot2") {
  if (!is.null(args$fill_color)) {
    if (geom %in% c("bar", "histogram", "boxplot", "violin", "density")) {
      args$fill <- args$fill_color %||% "#0C4C8A"
    }
    if (geom %in% c("line", "step", "point")) {
      args$colour <- args$fill_color %||% "#0C4C8A"
    }
  }
  if (geom %in% c("bar", "histogram", "boxplot", "violin", "density")) {
    args$size <- NULL
  }
  if (identical(args$position, "stack")) {
    args$position <- NULL
  }
  if (!geom %in% c("bar", "histogram")) {
    args$position <- NULL
  }
  pkg_envir <- getNamespace(envir)
  if (!grepl(pattern = "^geom_", x = geom))
    geom <- paste0("geom_", geom)
  geom_args <- try(formals(fun = get(geom, envir = pkg_envir)), silent = TRUE)
  if (inherits(geom_args, "try-error"))
    stop(paste(geom, "not found in", envir), call. = FALSE)
  if (!is.null(geom_args$stat)) {
    stat_args <- try(
      formals(fun = get(paste0("stat_", geom_args$stat), envir = pkg_envir)),
      silent = TRUE
    )
    if (!inherits(stat_args, "try-error")) {
      geom_args <- c(geom_args, stat_args)
    }
  }
  if (isTRUE(add_aes)) {
    GeomFun <- paste0("Geom", capitalize(gsub("geom_", "", geom)))
    GeomFun <- try(get(GeomFun, envir = pkg_envir), silent = TRUE)
    if (inherits(GeomFun, "try-error") & !is.null(geom_args$geom)) {
      GeomFun <- paste0("Geom", capitalize(geom_args$geom))
      GeomFun <- try(get(GeomFun, envir = pkg_envir), silent = TRUE)
    }
    if (!inherits(GeomFun, "try-error")) {
      aes_args <- GeomFun$aesthetics()
      geom_args <- c(geom_args, setNames(aes_args, aes_args))
    }
  }
  args[names(args) %in% setdiff(names(geom_args), names(mapping))]
}

