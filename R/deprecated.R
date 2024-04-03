
#' @title Deprecated functions
#' 
#' @param ... See [esquisse_container()]
#' 
#' @note The following functions are deprecated and will be removed in next release:
#'  * `esquisseContainer` : replaced by `esquisse_container`
#'  
#' @name esquisse-deprecated
#' @export
esquisseContainer <- function(...) {
  .Deprecated(new = "esquisse_container", package = "esquisse", old = "esquisseContainer")
  esquisse_container(...)
}

