
#' @importFrom scales hue_pal viridis_pal brewer_pal
default_pals <- function() {
  pals <- list(
    choices = list(
      Default = list("ggplot2" = hue_pal()(9)),
      Viridis = list(
        "viridis" = viridis_pal(option = "viridis")(10),
        "magma" = viridis_pal(option = "magma")(10),
        "inferno" = viridis_pal(option = "inferno")(10),
        "plasma" = viridis_pal(option = "plasma")(10),
        "cividis" = viridis_pal(option = "cividis")(10)
      ),
      Diverging = list(
        "BrBG" = brewer_pal(palette = "BrBG")(11), 
        "PiYG" = brewer_pal(palette = "PiYG")(11), 
        "PRGn" = brewer_pal(palette = "PRGn")(11), 
        "PuOr" = brewer_pal(palette = "PuOr")(11), 
        "RdBu" = brewer_pal(palette = "RdBu")(11), 
        "RdGy" = brewer_pal(palette = "RdGy")(11), 
        "RdYlBu" = brewer_pal(palette = "RdYlBu")(11), 
        "RdYlGn" = brewer_pal(palette = "RdYlGn")(11), 
        "Spectral" = brewer_pal(palette = "Spectral")(11)
      ), 
      Qualitative = list(
        "Accent" = brewer_pal(palette = "Accent")(8),
        "Dark2" = brewer_pal(palette = "Dark2")(8), 
        "Paired" = brewer_pal(palette = "Paired")(12), 
        "Pastel1" = brewer_pal(palette = "Pastel1")(9), 
        "Pastel2" = brewer_pal(palette = "Pastel2")(8), 
        "Set1" = brewer_pal(palette = "Set1")(8), 
        "Set2" = brewer_pal(palette = "Set2")(8), 
        "Set3" = brewer_pal(palette = "Set3")(12)
      ),
      Sequential = list(
        "Blues" = brewer_pal(palette = "Blues")(9),
        "BuGn" = brewer_pal(palette = "BuGn")(9),
        "BuPu" = brewer_pal(palette = "BuPu")(9), 
        "GnBu" = brewer_pal(palette = "GnBu")(9), 
        "Greens" = brewer_pal(palette = "Greens")(9), 
        "Greys" = brewer_pal(palette = "Greys")(9), 
        "Oranges" = brewer_pal(palette = "Oranges")(9), 
        "OrRd" = brewer_pal(palette = "OrRd")(9), 
        "PuBu" = brewer_pal(palette = "PuBu")(9), 
        "PuBuGn" = brewer_pal(palette = "PuBuGn")(9), 
        "PuRd" = brewer_pal(palette = "PuRd")(9), 
        "Purples" = brewer_pal(palette = "Purples")(9), 
        "RdPu" = brewer_pal(palette = "RdPu")(9), 
        "Reds" = brewer_pal(palette = "Reds")(9), 
        "YlGn" = brewer_pal(palette = "YlGn")(9), 
        "YlGnBu" = brewer_pal(palette = "YlGnBu")(9), 
        "YlOrBr" = brewer_pal(palette = "YlOrBr")(9), 
        "YlOrRd" = brewer_pal(palette = "YlOrRd")(9)
      )
    ), 
    textColor = c(
      rep(c("white", "black"), times = c(23, 18))
    )
  )
  return(pals)
}


#' @importFrom ggplot2 theme_bw theme_classic theme_dark theme_gray theme_grey 
#'  theme_light theme_linedraw theme_minimal theme_void
default_themes <- function() {
  
  ggplot2 <- c("bw", "classic", "dark", "gray",
               "light", "linedraw", "minimal",
               "void")
  ggplot2 <- setNames(as.list(paste0("theme_", ggplot2)), ggplot2)
  
  themes <- list(
    ggplot2 = ggplot2
  )
  if (requireNamespace("ggthemes", quietly = TRUE)) {
    ggthemes <- c(
      "base", "calc", "economist", "economist_white",
      "excel", "few", "fivethirtyeight", "foundation",
      "gdocs", "hc", "igray", "map", "pander",
      "par", "solarized", "solarized_2", "solid",
      "stata", "tufte", "wsj"
    )
    ggthemes <- setNames(as.list(paste0("ggthemes::theme_", ggthemes)), ggthemes)
    themes$ggthemes <- ggthemes
  }
  
  if (requireNamespace("hrbrthemes", quietly = TRUE)) {
    hrbrthemes <- c(
      "ft_rc", "ipsum", "ipsum_ps", "ipsum_rc", "ipsum_tw", "modern_rc"
    )
    hrbrthemes <- setNames(as.list(paste0("hrbrthemes::theme_", hrbrthemes)), hrbrthemes)
    themes$hrbrthemes <- hrbrthemes
  }
  
  return(themes)
}

check_theme_exist <- function(x, package = "ggplot2") {
  vapply(X = x, FUN = function(fun) {
    if (grepl(pattern = "::", x = fun)) {
      x <- strsplit(x = fun, split = "::")[[1]]
      fun <- x[2]
      package <- x[1]
      exists(fun, where = asNamespace(package), mode = "function")
    } else {
      if (!startsWith(fun, "theme_"))
        fun <- paste0("theme_", fun)
      exists(fun, where = asNamespace(package), mode = "function")
    }
  }, FUN.VALUE = logical(1), USE.NAMES = FALSE)
}



#' @importFrom scales viridis_pal brewer_pal
default_cols <- function() {
  cols <- list(
    "custom" = c("#0C4C8A", "#EF562D", "forestgreen", "steelblue", "firebrick", "darkorange", "hotpink"),
    "viridis" = viridis_pal(option = "viridis")(8),
    "plasma" = viridis_pal(option = "plasma")(8),
    "Blues" = brewer_pal(palette = "Blues")(9)[-1],
    "Greens" = brewer_pal(palette = "Greens")(9)[-1],
    "Reds" = brewer_pal(palette = "Reds")(9)[-1],
    "Greys" = brewer_pal(palette = "Greys")(9)[-1]
  )
  return(cols)
}
