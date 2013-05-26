#' @include MatrixList-class.R
NULL

setClassUnion("MatrixOrMatrixList", c("Matrix", "MatrixList"))

MatrixOrMatrixList <- function(...) {
  # If length ==1 and args[[1]] ==
  args <- list(...)
  if (length(args) == 1L) {
    if (is(args[[1]], "list")) {
      MatrixList(args[[1]])
    } else {
      Matrix(args[[1]])
    }
  } else {
    MatrixList(args)
  }
}

