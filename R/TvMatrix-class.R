# Wrap accessors to Matrix Class
## setClass("TvMatrix", contains = c("MatrixOrMatrixList"))

## setMethod("[[",
##           function(x, i, j) {
##             if (is(x, "Matrix")) x else x[[i]]
##           })

## setClass("TvMatrix2",
##          representation =
##          c(base = "Matrix", tv = "MatrixList"))

## setMethod("[[", c(x = "TvMatrix2"),
##           function(x, i, j) {
##             if (length(x@tv)) {
##               if (i > length(x@tv) || x < 1) {
##                 NULL 
##               } else {
##                 x@base + x@tv
##               }
##             } else {
##               x@base
##             }
##           })

## setMethod("[[<-", c(x = "TvMatrix", i = "Matrix"),
##           function(x, i, j) {
##             if (dim(i) != dim(x)) {
##               stop("dim(i) != dim(x)")
##             }
##             x@tv[[i]] <- Matrix(i - x@base)
##             is.null(x@tv) <- Matrix(0, nrow = nrow(x@base), ncol = ncol(x@base))
##             x
##           })

## setMethod("dim", c(x = "TvMatrix"),
##           function(x) {
##             dim(x@base)
##           })

## setMethod("length", c(x = "TvMatrix2"),
##           function(x) length(x@tv))

