
#' @importFrom scales hue_pal viridis_pal brewer_pal
#' @importFrom hrbrthemes ipsum_pal ft_pal
default_pals <- list(
  choices = list(
    Default = list("ggplot2" = hue_pal()(9)),
    Viridis = list(
      "viridis" = viridis_pal(option = "viridis")(10),
      "magma" = viridis_pal(option = "magma")(10),
      "inferno" = viridis_pal(option = "inferno")(10),
      "plasma" = viridis_pal(option = "plasma")(10),
      "cividis" = viridis_pal(option = "cividis")(10)
    ),
    hrbrthemes = list(
      "ipsum" = hrbrthemes::ipsum_pal()(9),
      "ft" = hrbrthemes::ft_pal()(9)
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
    rep(c("white", "black"), times = c(25, 18))
  )
)







