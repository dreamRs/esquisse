#' Utility to export ggplot objects to PowerPoint
#'
#' You can use the RStudio addin to interactively select ggplot objets, or
#' directly pass them to the function.
#'
#' @param gg character. Name(s) of ggplot object(s), if NULL, launch the Shiny gadget
#'
#' @return Path to the temporary ppt file.
#' @export
#'
#' @importFrom officer read_pptx add_slide
#' @importFrom rvg ph_with_vg
#' @importFrom utils browseURL
#' @importFrom shiny actionButton icon observeEvent dialogViewer runGadget stopApp actionLink
#' @importFrom miniUI miniPage miniContentPanel miniButtonBlock
#' @importFrom shinyWidgets pickerInput updateProgressBar progressBar prettyCheckboxGroup
#' @importFrom ggplot2 ggplot_build
#' @importMethodsFrom ggplot2 print.ggplot2
#'
#' @examples
#' \dontrun{
#'
#' # Shiny gadget
#' if (interactive()) {
#'
#' ggplot_to_ppt()
#'
#' }
#'
#' # Or with an object's name
#' library(ggplot2)
#' p <- ggplot(iris) +
#'   geom_point(aes(Sepal.Length, Sepal.Width))
#'
#' ggplot_to_ppt("p")
#'
#' }
ggplot_to_ppt <- function(gg = NULL) {

  # temp file to create ppt
  tmp <- tempfile(pattern = "charter", fileext = ".pptx")

  # get ggplots objects
  ggplots <- search_obj(what = "ggplot")

  # if no ggplot dont go further
  if (is.null(ggplots)) {
    message("No ggplot object in environment...")
    return(invisible())
  }

  if (!is.null(gg)) {

    if (!is.character(gg))
      stop("You must supply the names of the objects")

    if (!all(gg %in% ggplots))
      stop("Not all valid ggplot")

    ppt <- officer::read_pptx()

    for (ggg in gg) {
      ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
      testgg <- try(invisible(ggplot2::ggplot_build(get(ggg, envir = globalenv()))), silent = TRUE)
      if (!"try-error" %in% class(testgg)) {
        ppt <- rvg::ph_with_vg(ppt, print(get(ggg, envir = globalenv())), type = "body")
      } else {
        warning(paste0("Skipping '", ggg, "' because of : ", attr(testgg, "condition")$message))
      }
    }
    print(ppt, target = tmp)

    utils::browseURL(url = tmp)

  } else {

    ui <- miniUI::miniPage(
      toggleBtnUi(),
      miniUI::miniContentPanel(
        # shinyWidgets::pickerInput(
        #   inputId = "select_gg", label = "ggplot to export:",
        #   choices = ggplots,
        #   multiple = TRUE, width = "100%",
        #   options = list(
        #     `actions-box` = TRUE,
        #     `selected-text-format`= "count > 4", size = 5,
        #     `count-selected-text` = "{0} ggplot choosed (on a total of {1})"
        #   )
        # ),
        shinyWidgets::prettyCheckboxGroup(
          inputId = "select_gg", 
          label = tags$span("ggplot(s) to export ", shiny::actionLink(inputId = "all", label = "(select all)")), 
          choices = ggplots, status = "primary", 
          icon = icon("check")
        ),
        htmltools::tags$div(
          id = "ppt-pb", style = "display: none;",
          shinyWidgets::progressBar(id = "progress-ppt", value = 0, display_pct = TRUE)
        ),
        toggleDisplayUi(),
        htmltools::tags$script("$(function() {$('#select_gg').selectpicker('toggle');});")
      ),
      miniUI::miniButtonBlock(
        shiny::actionButton(
          inputId = "export", label = "Export",
          icon = shiny::icon("file-powerpoint-o"),
          class = "btn-block btn-primary"
        )
      )
    )

    server <- function(input, output, session) {
      
      shiny::observeEvent(input$all, {
        shinyWidgets::updatePrettyCheckboxGroup(
          session = session, inputId = "select_gg", selected = ggplots
        )
      })

      shiny::observeEvent(input$select_gg, {
        if (length(input$select_gg) > 0) {
          toggleBtnServer(session = session, inputId = "export", type = "enable")
        } else {
          toggleBtnServer(session = session, inputId = "export", type = "disable")
        }
      }, ignoreNULL = FALSE)

      shiny::observeEvent(input$export, {

        toggleDisplayServer(session = session, id = "ppt-pb", display = "block")

        if (length(input$select_gg) > 0) {

          total <- 2 + 2*length(input$select_gg)
          count <- 1

          ppt <- officer::read_pptx()
          shinyWidgets::updateProgressBar(session = session, id = "progress-ppt", value = count/total*100)
          count <- count + 1

          for (ggg in input$select_gg) {
            ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
            shinyWidgets::updateProgressBar(session = session, id = "progress-ppt", value = count/total*100)
            count <- count + 1
            # ppt <- rvg::ph_with_vg(ppt, print(get(ggg, envir = globalenv())), type = "body")
            testgg <- try(invisible(ggplot2::ggplot_build(get(ggg, envir = globalenv()))), silent = TRUE)
            if (!"try-error" %in% class(testgg)) {
              ppt <- rvg::ph_with_vg(ppt, print(get(ggg, envir = globalenv())), type = "body")
            } else {
              warning(paste0("Skipping '", ggg, "' because of : ", attr(testgg, "condition")$message))
            }
            shinyWidgets::updateProgressBar(session = session, id = "progress-ppt", value = count/total*100)
            count <- count + 1
          }

          print(ppt, target = tmp)

          shinyWidgets::updateProgressBar(session = session, id = "progress-ppt", value = count/total*100)

          utils::browseURL(url = tmp)

          shiny::stopApp()
        }

      })

    }

    inviewer <- shiny::dialogViewer(
      "Explort your ggplot2 to PowerPoint",
      width = 450, height = 180
    )
    shiny::runGadget(app = ui, server = server, viewer = inviewer)

  }

}


