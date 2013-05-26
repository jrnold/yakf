#' @include utilities.R
#' @include MatrixOrMatrixList-class.R
#' @exportClass SSM
#' @export SSM
NULL

#' @docType class
#' @rdname SSM-class
#' @aliases SSM-class
#' @aliases +,SSM,SSM-method
#' @title SSM class
#'
#' @description The \code{"SSM"} class represents a dynamic linear model,
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
#' @param kappa Default multiplier for diagonal values of \code{P1}, if \code{P1 = NULL}.
#' @return An object of class \code{\linkS4class{SSM}}.
#' @seealso \code{\linkS4class{SSM}}
#'
#' @section Slots:
#'
#' \describe{
#' \item{\code{T}}{\code{"MatrixOrMatrixList"}. The \eqn{T} matrix.}
#' \item{\code{Z}}{\code{"MatrixOrMatrixList"}. The \eqn{Z} matrix.}
#' \item{\code{H}}{\code{"MatrixOrMatrixList"}. The \eqn{H} matrix.}
#' \item{\code{Q}}{\code{"MatrixOrMatrixList"}. The \eqn{Q} matrix.}
#' \item{\code{R}}{\code{"MatrixOrMatrixList"}. The \eqn{R} matrix.}
#' \item{\code{P1}}{\code{"MatrixOrMatrixList"}. The \eqn{P_1} matrix with the covariance of the initial states.}
#' \item{\code{a1}}{\code{"MatrixOrMatrixList"}. The \eqn{a_1} vector with the mean of initial states.}
#' \item{\code{dd}}{\code{"MatrixOrMatrixList"}. The \eqn{d} vector.}
#' \item{\code{cc}}{\code{"MatrixOrMatrixList"}. The \eqn{c} vector.}
#' }
#'
#' @section Details:
#'
#' The structure and notation of state spece models represented this class is based on that used in SsfPack
#' and \pkg{dlm}. See the package vignette for details.
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
setClass("SSM",
         representation(T = "MatrixOrMatrixList",
                        Z = "MatrixOrMatrixList",
                        H = "MatrixOrMatrixList",
                        Q = "MatrixOrMatrixList",
                        R = "MatrixOrMatrixList",
                        P1 = "Matrix",
                        a1 = "Matrix",
                        cc = "MatrixOrMatrixList",
                        dd = "MatrixOrMatrixList"))

validity.SSM <- function(object) {
  # Matrix Dimensions
  # number of states    
  m <- nrow(object@T)
  # number of columns
  p <- nrow(object@Z)
  # number of disturbances
  r <- nrow(object@Q)
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
    x_dim <- dim(x)
    if (! identical(x_dim, expected)) {
      return(sprintf("Dimensions of slot %s != c(%d, %d)",
                     sQuote(slotname), expected[1], expected[2]))
    }
  }

  # Check that all time-varying matrices have the same length
  tv_mat <- sapply(names(matdims), function(x) is(slot(object, x), "MatrixList"))
  if (any(tv_mat) && ! thesame(sapply(names(matdims)[tv_mat],
                                      function(x) length(slot(object, x))))) {
    return("Not all time-varying system matrices have the same length")
  }
  
  # Check for symmetric matrices
  ## for (name in c("H", "G", "P")) {
  ##   if (! isSymmetric(slot(object, name))) {
  ##     return(sprintf("Matrix in slot %s is not symmetric", name))
  ##   }
  ## }
  TRUE
}
         
setValidity("SSM", validity.SSM)

#' @rdname SSM-class
SSM <- function(T, Z, H, Q,
                R = NULL, a1 = NULL, P1 = NULL, cc = NULL, dd = NULL,
                kappa = 10e6) {
  T = MatrixOrMatrixList(T)
  H = MatrixOrMatrixList(H)
  Z = MatrixOrMatrixList(Z)
  Q = MatrixOrMatrixList(Q)
  if (is.null(R)) {
    R <- Matrix(diag(nrow(Q)))
  }
  if (is.null(a1)) {
    a1 <-Matrix(rep(0, nrow(T)))
  }
  if (is.null(P1)) {
    P1 <-Matrix(diag(nrow(T)) * max(diag(Q)) * kappa)
  }
  if (is.null(cc)) {
    cc <-Matrix(0, nrow=nrow(Z))
  }
  if (is.null(dd)) {
    dd <- Matrix(0, nrow=nrow(T))
  }
  new("SSM", T = T, Z = Z, H = H, Q = Q, R = R,
      a1 = a1, P1 = P1, cc = cc, dd = dd)
}
