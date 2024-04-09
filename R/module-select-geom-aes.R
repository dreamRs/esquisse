
#' @importFrom shiny NS
#' @importFrom bslib nav_panel navset_pill
#' @importFrom htmltools tags tagList
select_geom_aes_ui <- function(id, n_geoms = 1, list_geoms = NULL) {

  if (is.null(list_geoms)) {
    list_geoms <- rep_len(list(geomIcons()), n_geoms)
  }

  if (!is_list(list_geoms)) {
    list_geoms <- rep_len(list(list_geoms), n_geoms)
  }

  ns <- NS(id)
  if (n_geoms == 1) {
    tags$div(
      class = "esquisse-geom-aes-main esquisse-geom-aes",
      tags$div(
        style = "padding: 3px 3px 0 3px; height: 122px;",
        dropInput(
          inputId = ns("geom_1"),
          choicesNames = list_geoms[[1]]$names,
          choicesValues = list_geoms[[1]]$values,
          dropWidth = "292px",
          width = "100%"
        )
      ),
      select_aes_ui(ns("aes_1"))
    )
  } else {
    navs_geom <- lapply(
      X = seq_len(n_geoms),
      FUN = function(i) {
        nav_panel(
          paste0("Geom #", i),
          tags$div(
            class = "esquisse-geom-aes",
            tags$div(
              style = "padding: 3px 3px 0 3px; height: 122px;",
              dropInput(
                inputId = ns(paste0("geom_", i)),
                choicesNames = list_geoms[[i]]$names,
                choicesValues = list_geoms[[i]]$values,
                dropWidth = "292px",
                width = "100%"
              )
            ),
            select_aes_ui(ns(paste0("aes_", i)))
          )
        )
      }
    )
    tags$div(
      class = "esquisse-geom-aes-main",
      navset_pill(
        id = ns("navset_geoms"),
        !!!navs_geom
      )
    )
  }
}

#' @importFrom shiny reactive moduleServer reactiveValues observeEvent reactiveValuesToList
select_geom_aes_server <- function(id,
                                   n_geoms = 1,
                                   data_r = reactive(NULL),
                                   default_aes = c("fill", "color", "size", "group", "facet"),
                                   aesthetics_r = reactive(NULL),
                                   geom_rv = reactiveValues()) {
  moduleServer(
    id,
    function(input, output, session) {

      rv <- reactiveValues()

      lapply(
        X = seq_len(n_geoms),
        FUN = function(i) {

          aes_r <- select_aes_server(
            id = paste0("aes_", i),
            data_r = data_r,
            default_aes = default_aes,
            input_aes = aesthetics_r
          )

          observeEvent(aes_r(), {
            rv[[paste0("aes_", i)]] <- aes_r()
          })

          # special case: geom_sf
          observeEvent(data_r(), {
            if (inherits(data_r(), what = "sf")) {
              geom_rv$possible <- c("sf", geom_rv$possible)
            }
          })

          bindEvent(observe({
            aesthetics <- rv[[paste0("aes_", i)]]
            data <- data_r()
            geoms <- potential_geoms(
              data = data,
              mapping = build_aes(
                data = data,
                # x = aesthetics$xvar,
                # y = aesthetics$yvar
                .list = aesthetics
              )
            )

            if (i == 1) {
              geom_rv$possible <- c("auto", geoms)
              geom_rv$controls <- select_geom_controls(input[[paste0("geom_", i)]], geoms)
              geom_rv$palette <- !is.null(aesthetics$fill) | !is.null(aesthetics$color)
            }
            rv[[paste0("geom_possible", i)]] <- c("auto", geoms)


          }), rv[[paste0("aes_", i)]], input[[paste0("geom_", i)]])

          observeEvent(rv[[paste0("geom_possible", i)]], {
            geoms <- geomIcons()$values
            geomposs <- rv[[paste0("geom_possible", i)]]
            updateDropInput(
              session = session,
              inputId = paste0("geom_", i),
              selected = setdiff(geomposs, "auto")[1],
              disabled = setdiff(geoms, geomposs)
            )
          })

          observeEvent(input[[paste0("geom_", i)]], {
            rv[[paste0("geom_", i)]] <- input[[paste0("geom_", i)]]
          })
        }
      )

      return(reactive({
        others <- reactiveValuesToList(rv)
        others$aes_1 <- NULL
        others$geom_1 <- NULL
        others[vapply(others, FUN = identical, "auto", FUN.VALUE = logical(1))] <- NULL
        result <- list(
          main = list(aes = rv$aes_1, geom = rv$geom_1),
          others = dropNullsOrEmpty(others)
        )
        result$active <- input$navset_geoms
        return(result)
      }))
    }
  )
}

