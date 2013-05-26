#' @include MatrixList-class.R
#' @exportMethod dim
NULL

dim.MatrixList <- function(x) {
  if (length(x)) {
    dim(x[[1]])
  } else {
    rep(0L, 2)
  }
}

setMethod("dim", c(x = "MatrixList"), dim.MatrixList)

