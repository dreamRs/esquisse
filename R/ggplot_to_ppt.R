#' Utility to export ggplot objects to PowerPoint
#'
#' You can use the RStudio addin to interactively select ggplot objects, or
#' directly pass their names to the function.
#'
#' @param gg character. Name(s) of ggplot object(s), if NULL, launch the Shiny gadget.
#'
#' @return Path to the temporary PowerPoint file.
#' @export
#'
#' @importFrom utils browseURL
#' @importFrom shiny actionButton icon observeEvent dialogViewer runGadget stopApp actionLink
#' @importFrom miniUI miniPage miniContentPanel miniButtonBlock
#' @importFrom shinyWidgets updateProgressBar progressBar prettyCheckboxGroup updatePrettyCheckboxGroup
#' @importFrom ggplot2 ggplot_build
#'
#' @examples
#'
#' # Shiny gadget
#' if (interactive()) {
#'
#' ggplot_to_ppt()
#'
#' 
#'
#' # Or with an object's name
#' library(ggplot2)
#' p <- ggplot(iris) +
#'   geom_point(aes(Sepal.Length, Sepal.Width))
#'
#' ggplot_to_ppt("p")
#' 
#' }
#'
ggplot_to_ppt <- function(gg = NULL) {
  
  if (!requireNamespace(package = "rvg"))
    message("Package 'rvg' is required to run this function")
  if (!requireNamespace(package = "officer"))
    message("Package 'officer' is required to run this function")

  # temp file to create ppt
  tmp <- tempfile(pattern = "esquisse", fileext = ".pptx")

  # get ggplots objects
  ggplots <- search_obj(what = "ggplot")

  # if no ggplot dont go further
  if (is.null(ggplots)) {
    message("No ggplot object in environment...")
    return(invisible())
  }

  if (!is.null(gg)) {

    if (!is.character(gg)) {
      stop("You must provide a character vector containing the names of plots to export", call. = FALSE)
    }

    if (!all(gg %in% ggplots)) {
      stop("Not all valid ggplot objects.", call. = FALSE)
    }

    ppt <- officer::read_pptx()

    for (ggg in gg) {
      ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
      testgg <- try(invisible(ggplot2::ggplot_build(get(ggg, envir = globalenv()))), silent = TRUE)
      if (!"try-error" %in% class(testgg)) {
        ppt <- rvg::ph_with_vg(ppt, print(get(ggg, envir = globalenv())), type = "body")
      } else {
        warning(paste0("Skipping '", ggg, "' because : ", attr(testgg, "condition")$message))
      }
    }
    print(ppt, target = tmp)

    browseURL(url = tmp)

  } else {

    ui <- miniPage(
      useShinyUtils(),
      miniContentPanel(
        prettyCheckboxGroup(
          inputId = "select_gg", 
          label = tags$span("ggplot(s) to export ", actionLink(inputId = "all", label = "(select all)")), 
          choices = ggplots, status = "primary", 
          icon = icon("check")
        ),
        tags$div(
          id = "ppt-pb", style = "display: none;",
          progressBar(id = "progress-ppt", value = 0, display_pct = TRUE)
        )
      ),
      miniButtonBlock(
        actionButton(
          inputId = "export", label = "Export",
          icon = icon("file-powerpoint-o"),
          class = "btn-block btn-primary"
        )
      )
    )

    server <- function(input, output, session) {
      
      observeEvent(input$all, {
        updatePrettyCheckboxGroup(
          session = session, inputId = "select_gg", selected = ggplots
        )
      })

      observeEvent(input$select_gg, {
        if (length(input$select_gg) > 0) {
          toggleBtn(session = session, inputId = "export", type = "enable")
        } else {
          toggleBtn(session = session, inputId = "export", type = "disable")
        }
      }, ignoreNULL = FALSE)

      observeEvent(input$export, {

        toggleDisplay(session = session, id = "ppt-pb", display = "block")

        if (length(input$select_gg) > 0) {

          total <- 2 + 2*length(input$select_gg)
          count <- 1

          ppt <- officer::read_pptx()
          updateProgressBar(session = session, id = "progress-ppt", value = count/total*100)
          count <- count + 1

          for (ggg in input$select_gg) {
            ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
            updateProgressBar(session = session, id = "progress-ppt", value = count/total*100)
            count <- count + 1
            # ppt <- rvg::ph_with_vg(ppt, print(get(ggg, envir = globalenv())), type = "body")
            testgg <- try(invisible(ggplot2::ggplot_build(get(ggg, envir = globalenv()))), silent = TRUE)
            if (!"try-error" %in% class(testgg)) {
              ppt <- rvg::ph_with_vg(ppt, print(get(ggg, envir = globalenv())), type = "body")
            } else {
              warning(paste0("Skipping '", ggg, "' because of : ", attr(testgg, "condition")$message))
            }
            updateProgressBar(session = session, id = "progress-ppt", value = count/total*100)
            count <- count + 1
          }

          print(ppt, target = tmp)

          updateProgressBar(session = session, id = "progress-ppt", value = count/total*100)

          utils::browseURL(url = tmp)

          stopApp()
        }

      })

    }

    inviewer <- dialogViewer(
      "Export your ggplot2 to PowerPoint",
      width = 450, height = 180
    )
    runGadget(app = ui, server = server, viewer = inviewer)

  }

}


