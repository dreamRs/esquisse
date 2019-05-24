% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/safe_ggplot.R
\name{safe_ggplot}
\alias{safe_ggplot}
\title{Safely render a \code{ggplot} in Shiny application}
\usage{
safe_ggplot(expr, data = NULL,
  session = shiny::getDefaultReactiveDomain())
}
\arguments{
\item{expr}{Code to produce a \code{ggplot} object.}

\item{data}{Argument passed to \code{\link[rlang]{eval_tidy}} to evaluate expression.}

\item{session}{Session object to send notification to.}
}
\value{
Output of \code{\link[ggplot2]{ggplot_build}}.
}
\description{
Safely render a \code{ggplot} in Shiny application
}
\examples{
if (interactive()) {
  library(shiny)
  library(ggplot2)
  
  ui <- fluidPage(
    fluidRow(
      column(
        width = 3,
        selectInput(
          inputId = "var", 
          label = "Var:", 
          choices = c("Sepal.Width", "Do.Not.Exist")
        )
      ),
      column(
        width = 9,
        plotOutput(outputId = "plot")
      )
    )
  )
  
  server <- function(input, output, session) {
    
    output$plot <- renderPlot({
      p <- ggplot(iris) +
        geom_point(aes_string("Sepal.Length", input$var))
      safe_ggplot(p)
    })
    
  }
  
  shinyApp(ui, server)
}
}