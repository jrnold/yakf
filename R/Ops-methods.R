#' @include MatrixList-class.R
#' @exportMethod Ops
NULL

setMethod("Ops", c(e1 = "MatrixList", e2 = "MatrixList"),
          function(e1, e2) {
            frame <- sys.parent()
            envir <- parent.frame()
            call <- sys.call(frame)
            fname <- as.character(call[[1L]])
            fdef <- get(fname, envir = envir)
            mapply(fdef, e1, e2)
          })

setMethod("Ops", c(e1 = "MatrixList", e2 = "Matrix"),
          function(e1, e2) {
            frame <- sys.parent()
            envir <- parent.frame()
            call <- sys.call(frame)
            fname <- as.character(call[[1L]])
            fdef <- get(fname, envir = envir)
            MatrixList(lapply(e1, function(x) fdef(x, e2)))
          })

setMethod("Ops", c(e1 = "Matrix", e2 = "MatrixList"),
          function(e1, e2) {
            callGeneric(e2, e1)
          })

