#' @include yakf-package.R
#' @exportClass KalmanFilter
NULL

#' @docType class
#' @title Kalman filter results
#'
#' @description Class containing results from a
#' Kalman filter, as produced by (\code{\link{kalman_filter}}).
#'
#' @section Slots:
#'
#' \describe{
#' \item{\code{v}}{\code{"Matrix"} of size N x n. The forecast errors.}
#' \item{\code{K}}{\code{"list"} of \code{"Matrix"} obects, each of dimension
#' N x m containing the Kalman gain for each observation.}
#' \item{\code{Finv}}{\code{"list"} of \code{"Matrix"} obects, each of dimension
#' N x N containing the \eqn{F^{-1}} matrices.}
#' }
#' @seealso \code{\link{kalman_filter}}, which returns objects of this class.
KalmanFilter <-
  setClass("KalmanFilter",
           representation(v = "Matrix",
                          K = "list",
                          Finv = "list"))

validity.KalmanFilter <- function(object) {
  N <- nrow(object@v)
  n <- ncol(object@v)
  if (length(object@K) != n) {
    return("ncol(K) != ncol(v)")
  }
  if (length(object@Finv) != n) {
    return("length(Finv) != ncol(v)")
  }
  # Check K
  if (!all(sapply(object@K, is, class2 = "Matrix"))) {
    return("class(x) != 'Matrix' for some element in K")
  }
  if (!all(sapply(object@K, function(x) ncol(x) == N))) {
    return("nrow(x) != nrow(v) for some element in K")
  }
  m <- nrow(object@K[[1]])
  if (!all(sapply(object@K, function(x) nrow(x) == m))) {
    return("Elements in K do not have equal rows")
  }

  # Check Finv
  if (!all(sapply(object@Finv, nrow, class2 = "Matrix"))) {
    return("class(x) != 'Matrix' for some element in Finv")
  }
  if (!all(sapply(object@Finv, function(x) nrow(x) == N))) {
    return("nrow(x) != 'N' for some element in Finv")
  }
  if (!all(sapply(object@Finv, function(x) ncol(x) == N))) {
    return("ncol(x) != 'N' for some element in Finv")
  }
  TRUE
}
