



#' @importFrom rstudioapi isAvailable getSourceEditorContext
get_data <- function(data = NULL, name = NULL) {
  
  if (!is.null(data)) {
    if (is.character(data)) {
      esquisse_data <- try({
        dat <- get(x = data, envir = globalenv())
        if (inherits(dat, what = "sf")) {
          dat
        } else {
          as.data.frame(dat)
        }
      }, silent = TRUE)
      esquisse_data_name <- data
      if ("try-error" %in% class(esquisse_data)) {
        warning(paste0("'", data, "' not found"), call. = FALSE)
        esquisse_data <- NULL
        esquisse_data_name <- ""
      }
    } else if (inherits(x = data, what = "data.frame")) {
      esquisse_data <- try({
        if (inherits(data, what = "sf")) {
          data
        } else {
          as.data.frame(data)
        }
      }, silent = TRUE)
      if ("try-error" %in% class(esquisse_data)) {
        warning(paste0("'", data, "' not found"), call. = FALSE)
        esquisse_data <- NULL
        esquisse_data_name <- ""
      } else {
        if (!is.null(name)) {
          esquisse_data_name <- as.character(name)
        } else {
          esquisse_data_name <- deparse(substitute(data))
        }
      }
      
      # esquisse_data_name <- gsub("\\[.*", "", esquisse_data_name)
    } else {
      esquisse_data <- NULL
      esquisse_data_name <- ""
    }
  } else {
    if (rstudioapi::isAvailable()) {
      context <- try(rstudioapi::getSourceEditorContext(), silent = TRUE)
      if ("try-error" %in% class(context) || is.null(context)) {
        esquisse_data <- NULL
        esquisse_data_name <- ""
      } else {
        context_select <- context$selection[[1]]$text
        if (isTRUE(nzchar(context_select))) {
          esquisse_data <- try(as.data.frame(get(x = context_select, envir = globalenv())), silent = TRUE)
          esquisse_data_name <- context_select
          if ("try-error" %in% class(esquisse_data)) {
            warning(paste0("Failed to retrieve data from the selection"), call. = FALSE)
            esquisse_data <- NULL
            esquisse_data_name <- ""
          }
        } else {
          esquisse_data <- NULL
          esquisse_data_name <- ""
        }
      }
    } else {
      esquisse_data <- NULL
      esquisse_data_name <- ""
    }
  }
  
  list(esquisse_data = esquisse_data, esquisse_data_name = esquisse_data_name)
}




