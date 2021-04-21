# esquisse 1.0.2

* Use named character vector for scale manual instead of list.



# esquisse 1.0.1

* Corrected a bug when using `sf` objects (fix [#147](https://github.com/dreamRs/esquisse/issues/147)).
* Use `pkg::data` notation if data used in addin comes from a package (fix [#150](https://github.com/dreamRs/esquisse/issues/150)).
* Corrected a bug when labs' input controls when the panel is disabled (fix [#148](https://github.com/dreamRs/esquisse/issues/148))



# esquisse 1.0.0

* Ability to select aesthetics parameters to use
* New module to import data from package [datamods](https://github.com/dreamRs/datamods)
* Possibility to define a manual color palette
* More format for exporting plots: png, pdf, svg, jpeg, pptx
* New function to render a `ggplot` and add export options: `ggplot_output()` / `render_ggplot()` 
* New module to export a plot: `save_ggplot_ui()` / `save_ggplot_server()`
* Options for setting title, subtitle, axis labels font size / weight, alignment
* Added geom step
* ability to select shape if aesthetic is used
* Allowing `geom_point` when plotting Time vs Continuous Data by [@matton2](https://github.com/matton2)



# esquisse 0.3.1

* facet wrap displays by [@xiangnandang](https://github.com/xiangnandang)
* facet_row and facet_col features to allow facet_grid call from ggplot2 by [@xiangnandang](https://github.com/xiangnandang)
* x and y limits of the plots by [@xiangnandang](https://github.com/xiangnandang)
* Check if data is null when changing datasets by [@trafficonese](https://github.com/trafficonese).
* New function `updateDragulaInput()` to update `dragulaInput()` server side.
* `dragulaInput()` has two new arguments: `ncolSource` and `ncolGrid` to create a grid layout with source and targets boxes.



# esquisse 0.3.0

* Fixed a bug in `dragulaInput` preventing to change variable in main addin.
* `chooseDataServer()` has a new argument `selectedTypes` to set types of variables selected by default. [#95](https://github.com/dreamRs/esquisse/issues/95)
* Packages [{ggthemes}](https://github.com/jrnold/ggthemes) and [{hrbrthemes}](https://github.com/hrbrmstr/hrbrthemes) are now in Suggests.



# esquisse 0.2.3

* New argument `disable_filters` in `esquisserUI()` to disable the ability to filter data.
* Enable bookmarking for module `filterDF`.
* `filterDF()` module handle missing values correctly.
* `filterDF()` has two new arguments : `drop_ids` : logical, drop or not column with only unique values, `picker` allow to use `shinyWidgets::pickerInput`.
* Code generated : no more affectation in {dplyr} code, a pipe is used to send code to {ggplot2} (mentioned in [#79](https://github.com/dreamRs/esquisse/issues/79))



# esquisse 0.2.2

* Added `esquisseContainer()` to better integrate esquisse module in a shiny application.
* New functions `colorPicker()` and `palettePicker()` to select a color or a palette (this is the select control used in the main addin).
* New argument `insert_code` in `esquisserUI()` to hide "Insert into script button".


## Breaking changes

* When using esquisse module into a shiny, it's not necessary anymore to put `esquisseUI` into a container, one is now added via argument `esquisseUI(container = ...)` :

    ```R
    # old
    tags$div(
      style = "height: 700px;",
      esquisserUI(
        id = "esquisse"
      )
    )
    
    # new 
    esquisserUI(
      id = "esquisse", 
      container = esquisseContainer(height = "700px")
    )
    ```


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
