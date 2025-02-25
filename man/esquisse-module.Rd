% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esquisse-ui.R, R/esquisse-server.R
\name{esquisse-module}
\alias{esquisse-module}
\alias{esquisse_ui}
\alias{esquisse_server}
\alias{esquisse_container}
\alias{esquisse_header}
\title{Esquisse module}
\usage{
esquisse_ui(
  id,
  header = esquisse_header(),
  container = esquisse_container(),
  controls = c("options", "labs", "axes", "geoms", "theme", "filters", "code"),
  insert_code = FALSE,
  play_pause = TRUE,
  layout_sidebar = FALSE,
  downloads = downloads_labels(),
  n_geoms = 8
)

esquisse_server(
  id,
  data_rv = NULL,
  name = "data",
  default_aes = c("fill", "color", "size", "group", "facet"),
  import_from = c("env", "file", "copypaste", "googlesheets", "url"),
  n_geoms = 8,
  drop_ids = TRUE,
  notify_warnings = NULL
)

esquisse_container(width = "100\%", height = "700px", fixed = FALSE)

esquisse_header(
  import_data = TRUE,
  show_data = TRUE,
  update_variable = TRUE,
  create_column = TRUE,
  cut_variable = TRUE,
  update_factor = TRUE,
  settings = TRUE,
  close = TRUE,
  .before = NULL,
  .after = NULL
)
}
\arguments{
\item{id}{Module ID.}

\item{header}{Either \code{TRUE} or \code{FALSE} to display or not \code{esquisse} header, or a named \code{list}
where names are : \code{settings}, \code{close}, \code{import} and \code{show_data} and values are \code{TRUE} or
\code{FALSE} to display or not the corresponding button.}

\item{container}{Container in which display the addin,
default is to use \code{\link[=esquisse_container]{esquisse_container()}}, see examples.
Use \code{NULL} for no container (behavior in versions <= 0.2.1).
Must be a \code{function}.}

\item{controls}{Controls menu to be displayed. Use \code{NULL} to hide all menus.}

\item{insert_code}{Logical, Display or not a button to insert the ggplot
code in the current user script (work only in RStudio).}

\item{play_pause}{Display or not the play / pause button.}

\item{layout_sidebar}{Put controls in a sidebar on the left rather than below the chart in dropdowns.}

\item{downloads}{Export options available or \code{NULL} for no export. See \code{\link[=downloads_labels]{downloads_labels()}}.}

\item{n_geoms}{Number of geoms the user can use.}

\item{data_rv}{Either:
\itemize{
\item A \code{\link[shiny:reactiveValues]{shiny::reactiveValues()}} with a slot \code{data} containing a \code{data.frame}
to use in the module and a slot \code{name} corresponding to the name of the \code{data.frame} used for the generated code.
\item A \code{\link[shiny:reactive]{shiny::reactive()}} function returning a \code{data.frame}. See argument \code{name} for the name used in generated code.
\item A \code{data.frame} object.
}}

\item{name}{The default name to use in generated code. Can be a \code{reactive} function return a single character.}

\item{default_aes}{Default aesthetics to be used, can be a \code{character}
vector or \code{reactive} function returning one.}

\item{import_from}{From where to import data, argument passed
to \code{\link[datamods:import-modal]{datamods::import_server()}}, use \code{NULL} to prevent the modal to appear.}

\item{drop_ids}{Argument passed to \link[datamods:filter-data]{datamods::filter_data_server}. Drop columns containing more than 90\% of unique values, or than 50 distinct values.}

\item{notify_warnings}{See \code{\link[=safe_ggplot]{safe_ggplot()}}. If \code{NULL}, the user can make his or her own choice via the settings menu, default is to show warnings once.}

\item{width, height}{The width and height of the container, e.g. \code{"400px"},
or \code{"100\%"}; see \code{\link[htmltools:validateCssUnit]{htmltools::validateCssUnit()}}.}

\item{fixed}{Use a fixed container, e.g. to use use esquisse full page.
If \code{TRUE}, width and height are ignored. Default to \code{FALSE}.
It's possible to use a vector of CSS unit of length 4 to specify the margins
(top, right, bottom, left).}

\item{import_data}{Show button to import data.}

\item{show_data}{Show button to display data.}

\item{update_variable}{Show button to update selected variables and convert them.}

\item{create_column}{Show button to create a new column based on an expression.}

\item{cut_variable}{Show button to allow to convert a numeric variable into factor.}

\item{update_factor}{Show button to open window to reorder factor levels and update them.}

\item{settings}{Show button to open settings modal (to select aesthetics to use).}

\item{close}{Show button to stop the app and close addin.}

\item{.before, .after}{Custom content to put in the header, typically buttons.}
}
\value{
A \code{reactiveValues} with 4 slots :
\itemize{
\item \strong{ggobj}: the ggplot object
\item \strong{code_plot}: code to generate plot.
\item \strong{code_filters}: a list of length two with code to reproduce filters.
\item \strong{data}: \code{data.frame} used in plot (with filters applied).
}
}
\description{
Use esquisse as a module in a Shiny application.
}
\examples{

### Part of a Shiny app ###

library(shiny)
library(esquisse)

ui <- fluidPage(

  theme = bs_theme_esquisse(),

  tags$h1("Use esquisse as a Shiny module"),

  radioButtons(
    inputId = "data",
    label = "Data to use:",
    choices = c("iris", "mtcars"),
    inline = TRUE
  ),
  checkboxGroupInput(
    inputId = "aes",
    label = "Aesthetics to use:",
    choices = c(
      "fill", "color", "size", "shape",
      "weight", "group", "facet", "facet_row", "facet_col"
    ),
    selected = c("fill", "color", "size", "facet"),
    inline = TRUE
  ),
  esquisse_ui(
    id = "esquisse",
    header = FALSE, # dont display gadget title
    container = esquisse_container(height = "700px")
  ),
  tags$b("Output of the module:"),
  verbatimTextOutput("out")
)

server <- function(input, output, session) {

  data_rv <- reactiveValues(data = iris, name = "iris")

  observeEvent(input$data, {
    if (input$data == "iris") {
      data_rv$data <- iris
      data_rv$name <- "iris"
    } else {
      data_rv$data <- mtcars
      data_rv$name <- "mtcars"
    }
  })

  esquisse_out <- esquisse_server(
    id = "esquisse",
    data_rv = data_rv,
    default_aes = reactive(input$aes)
  )

  output$out <- renderPrint({
    str(reactiveValuesToList(esquisse_out), max.level = 1)
  })
}

if (interactive())
  shinyApp(ui, server)


### Whole Shiny app ###

library(shiny)
library(esquisse)


# Load some datasets in app environment
my_data <- data.frame(
  var1 = rnorm(100),
  var2 = sample(letters[1:5], 100, TRUE)
)


ui <- fluidPage(
  theme = bs_theme_esquisse(),
  esquisse_ui(
    id = "esquisse",
    header = esquisse_header(
      close = FALSE, # hide the close button
      .after = actionButton( # custom button
        inputId = "open_modal",
        label = NULL,
        icon = icon("plus")
      )
    ),
    container = esquisse_container(fixed = TRUE),
    play_pause = FALSE,
    controls = c("settings", "labs", "axes", "geoms", "theme", "filters", "code", "export"),
    layout_sidebar = TRUE
  )
)

server <- function(input, output, session) {

  esquisse_server(id = "esquisse")


  observeEvent(input$open_modal, {
    showModal(modalDialog("Some content"))
  })
}

if (interactive())
  shinyApp(ui, server)



## You can also use a vector of margins for the fixed argument,
# useful if you have a navbar for example

library(shiny)
library(esquisse)
library(datamods)

ui <- navbarPage(
  title = "My navbar app",
  theme = bs_theme_esquisse(),
  tabPanel(
    title = "esquisse",
    esquisse_ui(
      id = "esquisse", 
      header = FALSE,
      container = esquisse_container(
        fixed = c(55, 0, 0, 0)
      )
    )
  )
)

server <- function(input, output, session) {
  
  # lauch import data modal
  import_modal(
    id = "import-data",
    from = c("env", "file", "copypaste"),
    title = "Import data"
  )
  data_imported_r <- datamods::import_server("import-data")

  data_rv <- reactiveValues(data = data.frame())
  observeEvent(data_imported_r$data(), {
    data_rv$data <- data_imported_r$data()
    data_rv$name <- data_imported_r$name()
  })
  
  esquisse_server(id = "esquisse", data_rv = data_rv)
  
}

if (interactive())
  shinyApp(ui, server)
}
