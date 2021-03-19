

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

`%empty%` <- function(a, b) {
  if (!is.null(a) && isTRUE(a != "")) a else b
}

`%|e|%` <- function(a, b) {
  if (!is.null(a) && a != "") a else b
}

`%nin%` <- Negate(`%in%`)


list1 <- function(x) {
  if (is.null(x))
    return(x)
  if (length(x) == 1 & !is.list(x)) {
    list(x)
  } else {
    x
  }
}


# utilities borrowed from shiny
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}
nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0
}
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE = logical(1))]
}
dropNullsOrEmptyRecursive <- function(x) {
  dropNullsOrEmpty(lapply(
    X = x, 
    FUN = function(x) {
      if (is.list(x)) {
        dropNullsOrEmpty(x)
      } else {
        x
      }
    }
  ))
}





#' Retrieve a data.frame by name from an environment
#'
#' @param df character, name of the object
#' @param env an environment
#'
#' @return the object
#' @noRd
#'
#' @importFrom utils data
#'
get_df <- function(df, env = globalenv()) {
  if (df %in% ls(name = env)) {
    get(x = df, envir = env)
  } else if (df %in% data(package = "ggplot2", envir = environment())$results[, "Item"]) {
    get(utils::data(list = df, package = "ggplot2", envir = environment()))
  } else {
    NULL
  }
}



#' Search for object with specific class in an environment
#'
#' @param what a class to look for
#' @param env An environment
#'
#' @return Character vector of the names of objects, NULL if none
#' @noRd
#'
#' @examples
#'
#' # NULL if no data.frame
#' search_obj("data.frame")
#'
#' library(ggplot2)
#' data("mpg")
#' search_obj("data.frame")
#'
#'
#' gg <- ggplot()
#' search_obj("ggplot")
#'
search_obj <- function(what = "data.frame", env = globalenv()) {
  all <- ls(name = env)
  objs <- lapply(
    X = all,
    FUN = function(x) {
      if (inherits(get(x, envir = env), what = what)) {
        x
      } else {
        NULL
      }
    }
  )
  objs <- unlist(objs)
  if (length(objs) == 1 && objs == "") {
    NULL
  } else {
    objs
  }
}


#' Create badge according to data type
#'
#' It uses conventions defined in the package, variable type are retrieve with \code{\link{col_type}}.
#'
#' @param col_name Variable's name
#' @param col_type Variable's type : 'discrete', 'time', 'continuous', 'id'
#'
#' @noRd
badgeType <- function(col_name, col_type) {
  stopifnot(length(col_name) == length(col_type))
  res <- lapply(
    X = seq_along(col_name),
    FUN = function(i) {
      col_name_i <- col_name[i]
      col_type_i <- col_type[i]
      if (col_type_i == "discrete") {
        tags$span(class = "label label-discrete badge-dad", col_name_i)
      } else if (col_type_i == "time") {
        tags$span(class = "label label-datetime badge-dad", col_name_i)
      } else if (col_type_i == "continuous") {
        tags$span(class="label label-continuous badge-dad", col_name_i)
      } else if (col_type_i == "id") {
        tags$span(class = "label label-default badge-dad", col_name_i)
      } else {
        tags$span(class = "label label-other badge-dad", col_name_i)
      }
    }
  )
  res
}





#' Try to guess type of a vector
#'
#' @param x a vector
#'
#' @noRd
col_type <- function(x, no_id = FALSE) {
  if (is.null(x))
    return(NULL)

  if (is.data.frame(x) && inherits(x, what = "sf")) {
    x <- x[, setdiff(names(x), attr(x, "sf_column")), drop = FALSE]
  }

  if (is.data.frame(x)) {
    return(unlist(lapply(x, col_type), use.names = FALSE))
  } else {
    if (inherits(x, c("logical", "character", "factor", "AsIs"))) {
      n <- length(x)
      u <- length(unique(x))
      if (u/n < 0.99 | u <= 30 | no_id) {
        return("discrete")
      } else {
        return("id")
      }
    }
    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
      return("time")
    }
    if (inherits(x, c("numeric", "integer", "double"))) {
      return("continuous")
    }
    return("unknown")
  }
  NULL
}

get_col_names <- function(data) {
  x <- setdiff(names(data), attr(data, "sf_column"))
  x[!vapply(data, is.list, FUN.VALUE = logical(1))]
}



# utils for geom icons
geomIcons <- function() {
  geoms <- c(
    "auto", "line", "step", "area", "bar", "histogram",
    "point", "boxplot", "violin", "density",
    "tile", "sf"
  )
  href <- "esquisse/geomIcon/gg-%s.png"
  geomsChoices <- lapply(
    X = geoms,
    FUN = function(x) {
      list(inputId = x, img = sprintf(fmt = href, x), label = capitalize(x))
    }
  )

  geomsChoicesNames <- lapply(
    X = geomsChoices,
    FUN = function(x) {
      list(
        style = "width: 90px;",
        tags$img(src = x$img, width = 56, height = 56),
        tags$br(), x$label
      )
    }
  )

  list(names = geomsChoicesNames, values = geoms)
}


capitalize <- function(x) {
  lo <- substring(text = x, first = 2)
  up <- substring(text = x, first = 1, last = 1)
  lo <- tolower(lo)
  up <- toupper(up)
  lo <- gsub(pattern = "_", replacement = " ", x = lo)
  paste0(up, lo)
}


dropListColumns <- function(x) {
  type_col <- vapply(X = x, FUN = typeof, FUN.VALUE = character(1), USE.NAMES = FALSE)
  x[, type_col != "list", drop = FALSE]
}







# colors ------------------------------------------------------------------



linear_gradient <- function(cols) {
  x <- round(seq(from = 0, to = 100, length.out = length(cols)+1))
  ind <- c(1, rep(seq_along(x)[-c(1, length(x))], each = 2), length(x))
  m <- matrix(data = paste0(x[ind], "%"), ncol = 2, byrow = TRUE)
  res <- lapply(
    X = seq_len(nrow(m)),
    FUN = function(i) {
      paste(paste(cols[i], m[i, 1]), paste(cols[i], m[i, 2]), sep = ", ")
    }
  )
  res <- unlist(res)
  res <- paste(res, collapse = ", ")
  paste0("linear-gradient(to right, ", res, ");")
}






# shiny utils -------------------------------------------------------------

normalizeChoicesArgs <- function(choices, choiceNames, choiceValues, mustExist = TRUE) {
  if (is.null(choices)) {
    if (is.null(choiceNames) || is.null(choiceValues)) {
      if (mustExist) {
        stop("Please specify a non-empty vector for `choices` (or, ",
             "alternatively, for both `choiceNames` AND `choiceValues`).")
      }
      else {
        if (is.null(choiceNames) && is.null(choiceValues)) {
          return(list(choiceNames = NULL, choiceValues = NULL))
        }
        else {
          stop("One of `choiceNames` or `choiceValues` was set to ",
               "NULL, but either both or none should be NULL.")
        }
      }
    }
    if (length(choiceNames) != length(choiceValues)) {
      stop("`choiceNames` and `choiceValues` must have the same length.")
    }
    if (anyNamed(choiceNames) || anyNamed(choiceValues)) {
      stop("`choiceNames` and `choiceValues` must not be named.")
    }
  }
  else {
    if (!is.null(choiceNames) || !is.null(choiceValues)) {
      warning("Using `choices` argument; ignoring `choiceNames` and `choiceValues`.")
    }
    choices <- choicesWithNames(choices)
    choiceNames <- names(choices)
    choiceValues <- unname(choices)
  }
  return(list(choiceNames = as.list(choiceNames), choiceValues = as.list(as.character(choiceValues))))
}


choicesWithNames <- function(choices) {
  listify <- function(obj) {
    makeNamed <- function(x) {
      if (is.null(names(x)))
        names(x) <- character(length(x))
      x
    }
    res <- lapply(obj, function(val) {
      if (is.list(val))
        listify(val)
      else if (length(val) == 1 && is.null(names(val)))
        val
      else makeNamed(as.list(val))
    })
    makeNamed(res)
  }
  choices <- listify(choices)
  if (length(choices) == 0)
    return(choices)
  choices <- mapply(choices, names(choices), FUN = function(choice,
                                                            name) {
    if (!is.list(choice))
      return(choice)
    if (name == "")
      stop("All sub-lists in \"choices\" must be named.")
    choicesWithNames(choice)
  }, SIMPLIFY = FALSE)
  missing <- names(choices) == ""
  names(choices)[missing] <- as.character(choices)[missing]
  choices
}


anyNamed <- function(x) {
  if (length(x) == 0)
    return(FALSE)
  nms <- names(x)
  if (is.null(nms))
    return(FALSE)
  any(nzchar(nms))
}

genId <- function(bytes = 12) {
  paste(format(as.hexmode(sample(256, bytes, replace = TRUE) - 1), width = 2), collapse = "")
}

makeId <- function(x) {
  x <- as.character(x)
  x <- lapply(X = x, FUN = function(y) {
    paste(as.character(charToRaw(y)), collapse = "")
  })
  x <- unlist(x, use.names = FALSE)
  make.unique(x, sep = "_")
}

idToChar <- function(x) {
  if (length(x) > 1) {
    x <- unlist(lapply(x, idToChar))
    return(x)
  }
  x <- strsplit(x, "")[[1]]
  x <- paste0(x[c(TRUE, FALSE)], x[c(FALSE, TRUE)])
  rawToChar(as.raw(as.hexmode(x)))
}

deparse2 <- function(x) {
  x <- deparse(x, width.cutoff = 100L)
  # x <- trimws(x)
  x <- paste(x, collapse = "\n")
  x <- gsub(x = x, pattern = "+", replacement = "+\n", fixed = TRUE)
  x <- gsub(x = x, pattern = "%>%", replacement = "%>%\n", fixed = TRUE)
  x <- gsub(x = x, pattern = "[ ]+", replacement = " ")
  x <- gsub(x = x, pattern = "\n \n", replacement = "\n")
  x <- gsub(x = x, pattern = "[\n]+", replacement = "\n")
  return(x)
}

style_code <- function(x) {
  code <- lapply(
    X = strsplit(x, split = "+", fixed = TRUE)[[1]],
    FUN = function(x) {
      # paste(expr_deparse(parse_expr(trimws(x)), width = 80), collapse = "\n  ")
      x <- rlang::expr_deparse(rlang::parse_expr(trimws(x)), width = 800)
      if (nchar(x) > 60) {
        # x <- sub(pattern = "(", replacement = "(\n    ", x = x, fixed = TRUE)
        # x <- sub(pattern = "\\)$", replacement = "\n  )", x = x)
        before <- sub(pattern = "\\(.*", replacement = "", x = x)
        l <- sub(pattern = "[^\\(]*\\(", replacement = "", x = x)
        l <- sub(pattern = "\\)$", replacement = "", x = l)
        s <- gsub(pattern = "[^[:space:]]", replacement = "", x = before)
        x <- paste(
          paste0(before, "("),
          paste0(s, " ", gsub(pattern = ",", replacement = paste0(",\n", s), x = l)),
          paste0(s, ")"),
          sep = "\n"
        )
      }
      x
    }
  )
  code <- Reduce(function(x, y) {
    paste(x, y, sep = " +\n  ")
  }, code)
  paste(as.character(styler::style_text(code)), collapse = "\n")
}
