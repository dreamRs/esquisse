# esquisse <img src="man/figures/logo_esquisse.png" width=200 align="right" />

> The purpose of this add-in is to let you explore your data quickly to extract the information they hold. You can create visualization with [{ggplot2}](https://ggplot2.tidyverse.org/), filter data with [{dplyr}](https://dplyr.tidyverse.org/) and retrieve generated code.

<!-- badges: start -->
[![version](http://www.r-pkg.org/badges/version/esquisse)](https://CRAN.R-project.org/package=esquisse)
[![cranlogs](http://cranlogs.r-pkg.org/badges/esquisse)](https://CRAN.R-project.org/package=esquisse)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Travis-CI Build Status](https://travis-ci.org/dreamRs/esquisse.svg?branch=master)](https://travis-ci.org/dreamRs/esquisse)
[![R build status](https://github.com/dreamRs/esquisse/workflows/R-CMD-check/badge.svg)](https://github.com/dreamRs/esquisse/actions)
<!-- badges: end -->

This addin allows you to interactively explore your data by visualizing it with the [ggplot2](https://github.com/tidyverse/ggplot2) package. It allows you to draw bar plots, curves, scatter plots, histograms, boxplot and [sf](https://github.com/r-spatial/sf) objects, then export the graph or retrieve the code to reproduce the graph.

See online documentation : https://dreamrs.github.io/esquisse/index.html

If you find bugs, please open an [issue](https://github.com/dreamRs/esquisse/issues)


## Installation

Install from CRAN with :

```r
install.packages("esquisse")
```

Or install development version from GitHub :

```r
remotes::install_github("dreamRs/esquisse")
```

Then launch the addin via the RStudio menu, if you don't have `data.frame` in your environment, datasets in `ggplot2` are used.


## `ggplot2` builder addin

![](man/figures/esquisse.gif)

Launch addin via RStudio menu or with:

```r
esquisse::esquisser()
```

