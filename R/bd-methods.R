#' @include MatrixList-class.R
#' @exportMethod %bd%
NULL

#' Block diagonal operator
#'
#' Special operator for the block diagonal of two matrices.
#'
setGeneric("%bd%", function(e1, e2) standardGeneric("%bd%"))

setMethod("%bd%", c(e1 = "MatrixList", e2 = "MatrixList"),
          function(e1, e2) {
            mapply(bdiag, e1, e2)
          })

setMethod("%bd%", c(e1 = "MatrixList", e2 = "Matrix"),
          function(e1, e2) {
            MatrixList(lapply(e1, function(x) bdiag(x, e2)))
          })

setMethod("%bd%", c(e1 = "Matrix", e2 = "MatrixList"),
          function(e1, e2) {
            MatrixList(lapply(e2, function(x) bdiag(e1, x)))
          })

setMethod("%bd%", c(e1 = "Matrix", e2 = "Matrix"),
          function(e1, e2) bdiag(e1, e2))

