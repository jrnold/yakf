#' @include MatrixList-class.R
#' @exportMethod cbind2
NULL

setMethod("cbind2", c(x = "MatrixList", y = "MatrixList"), 
          function(x, y) {
            mapply(cbind2, x, y)
          })

setMethod("cbind2", c(x = "MatrixList", y = "Matrix"), 
          function(x, y) {
            lapply(x, function(xi) cbind2(xi, y))
          })

setMethod("cbind2", c(x = "Matrix", y = "MatrixList"),
          function(x, y) {
            lapply(y, function(yi) cbind2(x, yi))
          })

