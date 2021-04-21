
#' Automatically select appropriate color scale
#'
#' @param mapping Aesthetics used in \code{ggplot}.
#' @param palette Color palette.
#' @param data An optional \code{data.frame} to choose the right type for variables.
#' @param fill_type,color_type Scale to use according to the variable used
#'  in \code{fill}/\code{color} aesthetic : \code{"discrete"} or \code{"continuous"}.
#'  Ignored if \code{data} is provided: it will be guessed from data.
#' @param reverse Reverse colors order or not.
#'
#' @return a \code{list}
#' @export
#' 
#' @importFrom ggplot2 scale_fill_hue scale_color_hue scale_fill_gradient scale_color_gradient
#'  scale_fill_brewer scale_color_brewer scale_fill_distiller scale_color_distiller
#'  scale_fill_viridis_c scale_color_viridis_c scale_fill_viridis_d scale_color_viridis_d
#' @importFrom rlang is_named
#' 
#' @examples
#' library(ggplot2)
#' 
#' # Automatic guess according to data
#' which_pal_scale(
#'   mapping = aes(fill = Sepal.Length), 
#'   palette = "ggplot2", 
#'   data = iris
#' )
#' which_pal_scale(
#'   mapping = aes(fill = Species),
#'   palette = "ggplot2", 
#'   data = iris
#' )
#' 
#' 
#' # Explicitly specify type
#' which_pal_scale(
#'   mapping = aes(color = variable), 
#'   palette = "Blues", 
#'   color_type = "discrete"
#' )
#' 
#' 
#' # Both scales
#' which_pal_scale(
#'   mapping = aes(color = var1, fill = var2), 
#'   palette = "Blues", 
#'   color_type = "discrete",
#'   fill_type = "continuous"
#' )
which_pal_scale <- function(mapping, 
                            palette = "ggplot2", 
                            data = NULL,
                            fill_type = c("continuous", "discrete"), 
                            color_type = c("continuous", "discrete"),
                            reverse = FALSE) {
  if (length(palette) < 1)
    return(list())
  args <- list()
  fill_type <- match.arg(fill_type)
  color_type <- match.arg(color_type)
  if (!is.null(data)) {
    data_mapped <- lapply(mapping, rlang::eval_tidy, data = data)
    if (inherits(x = data_mapped$fill, what = c("character", "factor"))) {
      fill_type <- "discrete"
    } else {
      fill_type <- "continuous"
    }
    if (inherits(x = data_mapped$colour, what = c("character", "factor"))) {
      color_type <- "discrete"
    } else {
      color_type <- "continuous"
    }
  }
  
  # Option 1: manual color palette
  if (rlang::is_named(palette)) {
    if (!is.null(mapping$fill)) {
      fill_scale <- switch(
        fill_type,
        "discrete" = "scale_fill_manual",
        "continuous" = "scale_fill_gradient"
      )
      args[[fill_scale]] <- switch(
        fill_type,
        "discrete" = list(values = unlist(palette, use.names = TRUE)),
        "continuous" = palette
      )
    } else {
      fill_scale <- NULL
    }
    if (!is.null(mapping$colour)) {
      color_scale <- switch(
        color_type,
        "discrete" = "scale_color_manual",
        "continuous" = "scale_color_gradient"
      )
      args[[color_scale]] <- switch(
        color_type,
        "discrete" = list(values = unlist(palette, use.names = TRUE)),
        "continuous" = palette
      )
    } else {
      color_scale <- NULL
    }
    return(list(
      scales = c(fill_scale, color_scale),
      args = args
    ))
  }
  
  # Option 2: known palette
  palettes <- unlist(lapply(default_pals()$choices, names), recursive = TRUE, use.names = FALSE)
  if (isTRUE(palette %in% palettes)) {
    scale_pal_d <- function(pal, aesthetic) {
      if (pal == "ggplot2") {
        s_p <- "hue"
      } else if (pal %in% c("viridis", "plasma", "magma", "cividis", "inferno")) {
        s_p <- "viridis_d"
      } else if (identical(pal, "ipsum")) {
        s_p <- "ipsum"
      } else if (identical(pal, "ft")) {
        s_p <- "ft"
      } else {
        s_p <- "brewer"
      }
      scl <- paste("scale", aesthetic, s_p, sep = "_")
      if (palette %in% c("ipsum", "ft")) {
        scl <- paste0("hrbrthemes::", scl)
      }
      return(scl)
    }
    scale_pal_c <- function(pal, aesthetic) {
      if (pal == "ggplot2") {
        s_p <- "gradient"
      } else if (pal %in% c("viridis", "plasma", "magma", "cividis", "inferno")) {
        s_p <- "viridis_c"
      } else if (identical(pal, "ipsum")) {
        s_p <- "ipsum"
      } else if (identical(pal, "ft")) {
        s_p <- "ft"
      } else {
        s_p <- "distiller"
      }
      scl <- paste("scale", aesthetic, s_p, sep = "_")
      if (palette %in% c("ipsum", "ft")) {
        scl <- paste0("hrbrthemes::", scl)
      }
      return(scl)
    }
    if (!is.null(mapping$fill)) {
      fill_scale <- switch(
        fill_type,
        "discrete" = scale_pal_d(palette, "fill"),
        "continuous" = scale_pal_c(palette, "fill")
      )
      if (!identical(palette, "ggplot2")) {
        args[[fill_scale]] <- setNames(
          object = list(palette), 
          nm = ifelse(grepl("viridis", fill_scale), "option", "palette")
        )
        if (palette %in% c("ipsum", "ft")) {
          args[[fill_scale]] <- NULL
        }
      }
      if (!endsWith(fill_scale, "gradient") & isTRUE(reverse)) {
        args[[fill_scale]] <- c(args[[fill_scale]], list(direction = -1))
      }
      if (!endsWith(fill_scale, "gradient") & !isTRUE(reverse)) {
        args[[fill_scale]] <- c(args[[fill_scale]], list(direction = 1))
      }
    } else {
      fill_scale <- NULL
    }
    if (!is.null(mapping$colour)) {
      color_scale <- switch(
        color_type,
        "discrete" = scale_pal_d(palette, "color"),
        "continuous" = scale_pal_c(palette, "color")
      )
      if (!identical(palette, "ggplot2")) {
        args[[color_scale]] <- setNames(
          object = list(palette), 
          nm = ifelse(grepl("viridis", color_scale), "option", "palette")
        )
        if (palette %in% c("ipsum", "ft")) {
          args[[color_scale]] <- NULL
        }
      }
      if (!endsWith(color_scale, "gradient") & isTRUE(reverse)) {
        args[[color_scale]] <- c(args[[color_scale]], list(direction = -1))
      }
      if (!endsWith(color_scale, "gradient") & !isTRUE(reverse)) {
        args[[color_scale]] <- c(args[[color_scale]], list(direction = 1))
      }
    } else {
      color_scale <- NULL
    }
    return(list(
      scales = c(fill_scale, color_scale),
      args = args
    ))
  }
  
  # Option 3: custom palette
  palettes <- get_palettes()$choices
  if (isTRUE(palette %in% names(palettes))) {
    palette <- palettes[[palette]]
  } else if (isTRUE(palette %in% names(Reduce(c, palettes)))) {
    palette <- Reduce(c, palettes)[[palette]]
  } else {
    stop("Unable to find palette ", palette, "!")
  }
  if (!is.null(mapping$fill)) {
    fill_scale <- switch(
      fill_type,
      "discrete" = "scale_fill_manual",
      "continuous" = "scale_fill_gradientn"
    )
    args[[fill_scale]] <- switch(
      fill_type,
      "discrete" = list(values = unlist(palette, use.names = FALSE)),
      "continuous" = list(colours = unlist(palette, use.names = FALSE))
    )
  } else {
    fill_scale <- NULL
  }
  if (!is.null(mapping$colour)) {
    color_scale <- switch(
      color_type,
      "discrete" = "scale_color_manual",
      "continuous" = "scale_color_gradientn"
    )
    args[[color_scale]] <- switch(
      color_type,
      "discrete" = list(values = unlist(palette, use.names = FALSE)),
      "continuous" = list(colours = unlist(palette, use.names = FALSE))
    )
  } else {
    color_scale <- NULL
  }
  return(list(
    scales = c(fill_scale, color_scale),
    args = args
  ))
}


