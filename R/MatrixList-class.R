#' @exportClass MatrixList
#" @export MatrixList
NULL

#' @rdname MatrixList-class
#' @title List of Matrix objects
#'
#' @description A list of \code{\linkS4class{Matrix}} objects, which must all
#' have the same dimensions.
#'
#' @section Extends:
#'
#' \description{
#' \item{\code{list}}{Directly.}
#' }
setClass("MatrixList", contains = "list")

MatrixList <- function(...) {
  args <- list(...)
  if (length(args) == 1L && is(args[[1]], "list")) {
    args <- args[[1]]
  }
  if (any(! sapply(args, is, class2 = "Matrix"))) {
    args <- lapply(args, Matrix)
  }
  new("MatrixList", args)
}

validity.MatrixList <- function(object) {
  if (! all(sapply(object, is, class2 = "Matrix"))) {
    return(sprintf("not all elements are %s objects", dQuote("Matrix")))
  }
  el_dim <- dim(object[[1]])
  if (! all(sapply(object, function(x) identical(dim(x), el_dim)))) {
    return("elements do not have equal dimension")
  }
  TRUE
}

setValidity("MatrixList", validity.MatrixList)

dim.MatrixList <- function(x) {
  if (length(x)) {
    dim(x[[1]])
  } else {
    rep(0L, 2)
  }
}

setMethod("dim", c(x = "MatrixList"), dim.MatrixList)

