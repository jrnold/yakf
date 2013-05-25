#' @include DLM-class.R
#' @exportClass DLM
#' @export DLM
NULL

MatrixList <- setClass("MatrixList", contains = "list")

validity.MatrixList <- function(object) {
  if (! all(sapply(object, is, class2 = "Matrix"))) {
    return(sprintf("not all elements are %s objects", dQuote("Matrix")))
  }
  el_dim <- dim(object[[1]])
  if (! all(identical(dim(object), el_dim))) {
    return("elements do not have equal dimension")
  }
  TRUE
}

setValidity("MatrixList", validity.MatrixList)

setClassUnion("ListOrMatrix", c("list", "Matrix"))

setClass("DlmMatrix",
         contains = "ListOrMatrix",
         prototype = Matrix(nrow = 0, ncol = 0))

DlmMatrix <- function(x) {
  if (is(x, "list")) {
    x <- lapply(x, Matrix)
  } else {
    x <- Matrix(x)
  }
  new("DlmMatrix", x)
}

setMethod("[[", c(x = "DlmMatrix"),
          function(x, i, j) {
            if (is(x, "list")) {
              x[[i]]
            } else {
              x
            }
          })

## Check if DlmMatrix is time varying
is_tv <- function(object) is(object, "list")

## check that all items in a sequence are identical
thesame <- function(x) {
  sum(!duplicated(x)) == 1
}

dimlen <- function(x) {
  if (is.null(dim(x))) {
    length(x)
  } else {
    dim(x)
  }
}

#' @docType class
#' @rdname DLM-class
#' @aliases DLM-class
#' @aliases +,DLM,DLM-method
#' @title DLM class
#'
#' @description The \code{"DLM"} class represents a dynamic linear model,
#' also called state-space model.
#'
#' @param T \code{Matrix} of dimension m x m.
#' @param Z \code{Matrix} of dimension p x m.
#' @param H \code{Matrix} of dimension m x m. 
#' @param R \code{Matrix} of dimension p x r.
#' @param Q \code{Matrix} of dimension r x r.
#' @param P1 \code{Matrix} of dimension m x m. If \code{NULL}, then
#' a diffuse diagonal Matrix is used. The diagonal of this Matrix is set to 10^6 times the
#' maximum of 1 or values along the diagonal of \code{HH} and \code{GG}.
#' @param a1 \code{Matrix} of length m. If \code{NULL}, then set to a 0 vector.
#' @param dd \code{Matrix} of dimension N. If \code{NULL}, then set to a 0 vector.
#' @param cc \code{Matrix} of dimension m. If \code{NULL}, then set to a 0 vector.
#' @param tv_T \code{matrix} Indices of time-varying elements in \code{T}. See details.
#' @param tv_Z \code{matrix} Indices of time-varying elements in \code{Z}. See details.
#' @param tv_H \code{matrix} Indices of time-varying elements in \code{H}. See details.
#' @param tv_R \code{matrix} Indices of time-varying elements in \code{R}. See details. 
#' @param tv_Q \code{matrix} Indices of time-varying elements in \code{Q}. See details. 
#' @param tv_cc \code{matrix} Indices of time-varying elements in \code{cc}. See details. 
#' @param tv_dd \code{matrix} Indices of time-varying elements in \code{dd}. See details. 
#' @param X \code{matrix} containing data used in time-varying parameters.
#' @return An object of class \code{\linkS4class{DLM}}.
#' @seealso \code{\linkS4class{DLM}}
#'
#' @section Slots:
#'
#' \describe{
#' \item{\code{T}}{\code{"Matrix"}. The \eqn{T} matrix.}
#' \item{\code{Z}}{\code{"Matrix"}. The \eqn{Z} matrix.}
#' \item{\code{H}}{\code{"Matrix"}. The \eqn{H} matrix.}
#' \item{\code{Q}}{\code{"Matrix"}. The \eqn{Q} matrix.}
#' \item{\code{R}}{\code{"Matrix"}. The \eqn{R} matrix.}
#' \item{\code{P1}}{\code{"Matrix"}. The \eqn{P_1} matrix with the covariance of the initial states.}
#' \item{\code{a1}}{\code{"Matrix"}. The \eqn{a_1} vector with the mean of initial states.}
#' \item{\code{dd}}{\code{"Matrix"}. The \eqn{d} vector.}
#' \item{\code{cc}}{\code{"Matrix"}. The \eqn{c} vector.}
#' \item{\code{tv_T}}{\code{"matrix"}. Time varying indices for the \code{T} slot. See details.}
#' \item{\code{tv_Z}}{\code{"matrix"}. Time varying indices for the \code{Z} slot. See details.}
#' \item{\code{tv_H}}{\code{"matrix"}. Time varying indices for the \code{H} slot. See details.}
#' \item{\code{tv_Q}}{\code{"matrix"}. Time varying indices for the \code{Q} slot. See details.}
#' \item{\code{tv_R}}{\code{"matrix"}. Time varying indices for the \code{R} slot. See details.}
#' \item{\code{tv_cc}}{\code{"matrix"}. Time varying indices for the \code{cc} slot. See details.}
#' \item{\code{tv_dd}}{\code{"matrix"}. Time varying indices for the \code{dd} slot. See details.}
#' \item{\code{tv_HG}}{\code{"matrix"}. Time varying indices for the \code{HG} slot. See details.}
#' \item{\code{X}}{\code{"Matrix"}. Contains the time-varying parameters data.}
#' }
#'
#' @section Details:
#'
#' The structure and notation of DLM models in this class is based on that used in SsfPack
#' and \pkg{dlm}. See the package vignette for details.
#'
#'
#' @references
#' 
#' Giovanni Petris (2010), An R Package for Dynamic Linear Models.
#  Journal of Statistical Software, 36(12), 1-16.  \url{http://www.jstatsoft.org/v36/i12/}
#'
#' Petris, Petrone, and Campagnoli, Dynamic Linear Models with R, Springer (2009).
#'     
#' West and Harrison, Bayesian forecasting and dynamic models (2nd ed.), Springer (1997).
#'
setClass("DLM",
         representation(T = "DlmMatrix",
                        Z = "DlmMatrix",
                        H = "DlmMatrix",
                        Q = "DlmMatrix",
                        R = "DlmMatrix",
                        P1 = "Matrix",
                        a1 = "Matrix"))

validity.DLM <- function(object) {
  # Matrix Dimensions
  m <- nrow(object@T[[1]]) # number of states
  p <- nrow(object@Z[[1]]) # number of columns
  r <- nrow(object@Q[[1]])
  matdims <- list(T = c(m, m),
                  Z = c(p, m),
                  H = c(p, p),
                  R = c(m, r),
                  Q = c(r, r),
                  a1 = c(m, 1L),
                  P1 = c(m, m),
                  cc = c(p, 1L),
                  dd = c(m, 1L))

  # Check that matrices have consistent dimensions
  for (i in seq_along(matdims)) {
    slotname <- names(matdims)[i]
    x <- slot(object, slotname)
    expected <- matdims[[i]]
    if (is(x, "Matrix")) {
      x_dim <- dim(x)
    } else {
      x_dim <- dim(x[[1]])
    }
    if (! identical(x_dim, expected)) {
      return(sprintf("Dimensions of slot %s != c(%d, %d)",
                     sQuote(slotname), expected[1], expectedp[2]))
    }
  }

  # Check that all time-varying matrices have the same length
  tv_mat <- sapply(names(matdims), function(x) is_tv(slot(object, x)))
  if (! thesame(sapply(names(matdims)[tv_mat],
                       function(x) length(slot(object, x))))) {
    return("Not all time-varying system matrices have the same length")
  }
  
  # Check for symmetric matrices
  for (name in c("H", "G", "P")) {
    if (! isSymmetric(slot(object, name))) {
      return(sprintf("Matrix in slot %s is not symmetric", name))
    }
  }
  TRUE
}
         
setValidity("DLM", validity.DLM)

empty_tv_idx <- function() matrix(nrow=0, ncol= 3)

#' @rdname DLM-class
DLM <- function(T, Z, H, Q,
                R = Matrix(diag(nrow(Q))),
                a1 = Matrix(rep(0, nrow(T))),
                P1 = Matrix(diag(nrow(T)) * max(diag(Q)) * kappa),
                cc = Matrix(0, nrow=nrow(T)),
                dd = Matrix(0, nrow=nrow(Z))) {
  new("DLM",
      T = DlmMatrix(T),
      H = DlmMatrix(H),
      Z = DlmMatrix(Z),
      Q = DlmMatrix(Q),
      R = DlmMatrix(R),
      a1 = Matrix(a1),
      P1 = Matrix(P1))
}
