#' @exportClass MatrixList
#' @export MatrixList
NULL

#' @name MatrixList-class
#' @rdname MatrixList-class
#' @docType class
#' @aliases MatrixList-class
#' @aliases Ops,MatrixList,MatrixList-method
#' @aliases Ops,MatrixList,Matrix-method
#' @aliases Ops,Matrix,MatrixList-method
#' @aliases rbind2,MatrixList,MatrixList-method
#' @aliases rbind2,MatrixList,Matrix-method
#' @aliases rbind2,Matrix,MatrixList-method
#' @aliases cbind2,MatrixList,MatrixList-method
#' @aliases cbind2,MatrixList,Matrix-method
#' @aliases cbind2,Matrix,MatrixList-method
#' @aliases dim,MatrixList-method
#' @aliases %bd%,MatrixList,MatrixList-method
#' @aliases %bd%,MatrixList,Matrix-method
#' @aliases %bd%,Matrix,MatrixList-method
#' @title List of Matrix objects
#'
#' @description A list of \code{\linkS4class{Matrix}} objects, which must all
#' have the same dimensions.
#'
#' @section Extends:
#'
#' \describe{
#' \item{\code{list}}{Directly.}
#' }
#'
#' @section Methods:
#'
#' Many of these methods simply apply a function elementwise to each matrix in the object, and work for both \code{"MatrixList"} objects and \code{"Matrix"} objects.
#'
#' \describe{
#'     \item{\%bd\%}{\code{signature(e1 = "MatrixList", e2 = "Matrix")}: Block diagonal of \code{e1} and \code{e2}.}
#'     \item{\%bd\%}{\code{signature(e1 = "MatrixList", e2 = "MatrixList")}: Block diagonal of \code{e1} and \code{e2}.}
#'     \item{\%bd\%}{\code{signature(e1 = "Matrix", e2 = "MatrixList")}: Block diagonal of \code{e1} and \code{e2}.}
#'     \item{cbind2}{\code{signature(x = "MatrixList", y = "Matrix")}: Bind columns of \code{e1} and \code{e2}.}
#'     \item{cbind2}{\code{signature(x = "MatrixList", y = "MatrixList")}: Bind columns of \code{e1} and \code{e2}.}
#'     \item{cbind2}{\code{signature(x = "Matrix", y = "MatrixList")}: Bind columns of \code{e1} and \code{e2}.}
#'     \item{dim}{\code{signature(x = "MatrixList")} : Dimensions of the matrix elements in the object. Use \code{length} to the get the number of matrices.}
#'     \item{Ops}{\code{signature(e1 = "MatrixList", e2 = "Matrix")}: Operators. These are applied elementwise to \code{e1} and \code{e2}.}
#'     \item{Ops}{\code{signature(e1 = "MatrixList", e2 = "MatrixList")}: Operators. These are applied elementwise to \code{e1} and \code{e2}.}
#'     \item{Ops}{\code{signature(e1 = "Matrix", e2 = "MatrixList")}: Operators. These are applied elementwise to \code{e1} and \code{e2}.}
#'     \item{rbind2}{\code{signature(x = "MatrixList", y = "Matrix")}: Bind rows of \code{e1} and \code{e2}.}
#'     \item{rbind2}{\code{signature(x = "MatrixList", y = "MatrixList")}: Bind rows of \code{e1} and \code{e2}.}
#'     \item{rbind2}{\code{signature(x = "Matrix", y = "MatrixList")}: Bind rows of \code{e1} and \code{e2}.}
#' 	 }
#'
#' @examples
#' library("Matrix")
#' # create a Matrix List of three 2 x 2 matrices
#' foo <- MatrixList(Matrix(1, 2, 2), Matrix(2, 2, 2), Matrix(3, 2, 2))
#' 
#' # create another list
#' bar <- MatrixList(replicate(length(foo), Matrix(5, 2, 2)))
#' # Some methods (defined for both MatrixList objects, and
#' # MatrixList, Matrix object combinations
#' foo - bar
#' foo + bar
#' foo[[1]] + bar
#' cBind(foo, bar)
#' rBind(foo, bar)
#' # block diagonal
#" foo %bd% bar
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
