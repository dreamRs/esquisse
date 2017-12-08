#' Module for selecting variables with drag and drop
#'
#' @param id Module's id
#' @param dragula_css Path to dragula's CSS
#' @param dragula_js Path to dragula's JS
#' @param bindings Bindings to dragula
#' @param styles Path to custom CSS
#'
#' @noRd
dragAndDropUi <- function(id, dragula_css, dragula_js, bindings, styles = NULL) {

  # Namespace
  ns <- NS(id)

  tagList(
    tags$head(
      # dragula
      tags$script(src = dragula_js),
      tags$link(rel = "stylesheet", type = "text/css", href = dragula_css),
      tags$script(src = bindings),
      # styles
      tags$link(rel = "stylesheet", type = "text/css", href = styles)
    ),

    fillCol(
      tags$div(style = "height: 95%;", class = "box-dad", uiOutput(outputId = ns("vars"))),
      fillRow(
        tags$div(
          style = "height: 95%;",
          class = "box-dad xyvar", id = ns("xvar"),
          tags$em(tags$b("X axis", class = "label-background"))
        ),
        tags$div(
          style = "height: 95%;",
          class = "box-dad xyvar", id = ns("yvar"),
          tags$em(tags$b("Y axis", class = "label-background"))
        ),
        tags$div(
          style = "height: 95%;",
          class = "box-dad xyvar", id = ns("fill"),
          tags$em(tags$b("fill", class = "label-background"))
        ),
        tags$div(
          style = "height: 95%;",
          class = "box-dad xyvar", id = ns("color"),
          tags$em(tags$b("color", class = "label-background"))
        ),
        tags$div(
          style = "height: 95%;",
          class = "box-dad xyvar", id = ns("size"),
          tags$em(tags$b("size", class = "label-background"))
        )
      )
    )
  )
}

dragIds <- function(xvar = "xvar", yvar = "yvar", group = "group", size = "size") {
  list(xvar = xvar, yvar = yvar, group = group, size = size)
}


# Ecoutez !
# Puisqu'on allume les etoiles,
# c'est qu'elles sont à
# quelqu'un necessaires ?
# C'est que quelqu'un desire
# qu'elles soient ?
# C'est que quelqu'un dit perles
# ces crachats ?
# Et, forçant la bourrasque a midi des poussieres,
# il fonce jusqu'a Dieu,
# craint d'arriver trop tard, pleure,
# baise sa main noueuse, implore
# il lui faut une etoile !
# jure qu'il ne peut supporter
# son martyre sans etoiles.
#
# Ensuite,
# il promene son angoisse,
# il fait semblant d'être calme.
# Il dit à quelqu'un :
# " Maintenant, tu vas mieux,
# n'est-ce pas ? T'as plus peur ? Dis ? "
#
# Ecoutez !
# Puisqu'on allume les etoiles,
# c'est qu'elles sont à quelqu'un necessaires ?
# c'est qu'il est indispensable,
# que tous les soirs
# au-dessus des toits
# se mette à luire seule au moins
# une etoile?

# Vladimir Maiakovski


#' Module for selecting variables with drag and drop
#'
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param data    data to variables from
#'
#' @importFrom shiny renderUI reactiveValues observeEvent reactiveValuesToList
#' @importFrom htmltools tagList tags HTML
#'
#' @return a reactiValues containing inputs
#' @noRd
dragAndDropServer <- function(input, output, session, data) {

  # Namespace
  ns <- session$ns

  output$vars <- shiny::renderUI({
    if (!is.null(data$x)) {
      res$x <- list()
      dat <- data$x
      # res_col_type <- unlist(dat[, lapply(.SD, col_type)])
      res_col_type <- unlist(lapply(dat, col_type))
      htmltools::tagList(
        htmltools::tags$em(tags$b("Variables", class = "label-background")),
        htmltools::tags$div(
          id = ns("variables"), style = "margin: 5px; min-width: 50px; min-height: 15px;",
          htmltools::HTML(paste(badgeType(col_name = names(res_col_type), col_type = unname(res_col_type)), collapse = " "))
        ),
        htmltools::tags$script(
          sprintf(
            "dragAndDrop('%s', '%s', '%s', '%s', '%s', '%s');",
            ns("variables"), ns("xvar"), ns("yvar"), ns("fill"), ns("color"), ns("size")
          )
        )
      )

    } else {

      htmltools::tags$em(class = "label-dad", "Please choose some data")

    }
  })


  res <- shiny::reactiveValues(x = NULL)
  shiny::observeEvent(shiny::reactiveValuesToList(input), {
    res$x <- shiny::reactiveValuesToList(input)
  })

  return(res)
}
