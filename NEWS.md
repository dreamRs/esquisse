# esquisse 0.2.2

* Added `esquisseContainer` to better integrate esquisse module in a shiny application.


# esquisse 0.2.1

* Fix bug when adding smooth line (missing import for `geom_smooth`).
* Support for themes and scales (color/fill) from [{hrbrthemes}](https://github.com/hrbrmstr/hrbrthemes).
* Set labels for fill, color and size aesthetics.


# esquisse 0.2.0

* Now use `rlang` to generate ggplot code.
* Code generated when filtering data is available above ggplot code, it use `dplyr` syntax. [#19](https://github.com/dreamRs/esquisse/issues/19), [#46](https://github.com/dreamRs/esquisse/issues/46)
* Ability to set `scales` argument in `facet_wrap` (fixed, free, free_x, free_y). [#47](https://github.com/dreamRs/esquisse/issues/47)
* Support for scales continuous transformation (log1p, log, sqrt, reverse, ...). [#24](https://github.com/dreamRs/esquisse/issues/24)


# esquisse 0.1.8

* Support for group aesthetic.
* New supported geom: `geom_area`.


# esquisse 0.1.7

* Play/Pause button to stop reactivity when creating a plot (prevent plot to update each time you make a change).
* Support for palette from [`viridisLite`](https://github.com/sjmgarnier/viridisLite).



# esquisse 0.1.6

* Support for `sf` objects.
* Use `esquisse` as a shiny module.
* Facets support thanks to [@itcarroll](https://github.com/itcarroll) ([#30](https://github.com/dreamRs/esquisse/pull/30))
* New import data modules.
* Fix a bug when launching addin in RStudio [#25](https://github.com/dreamRs/esquisse/issues/25)



# esquisse 0.1.5

* Module to coerce a variable to a different type.
* Scroll when data have a lot of variables.
* Prevent filter for discrete variables with over 100 unique elements.


# esquisse 0.1.0

* Addin to make ggplot.
