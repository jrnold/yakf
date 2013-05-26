#' @include MatrixList-class.R
#' @exportMethod rbind2
NULL

setMethod("rbind2", c(x = "MatrixList", y = "MatrixList"), 
          function(x, y) {
            mapply(rbind2, x, y)
          })

setMethod("rbind2", c(x = "MatrixList", y = "Matrix"), 
          function(x, y) {
            lapply(x, function(xi) rbind2(xi, y))
          })

setMethod("rbind2", c(x = "Matrix", y = "MatrixList"),
          function(x, y) {
            lapply(y, function(yi) rbind2(x, yi))
          })

