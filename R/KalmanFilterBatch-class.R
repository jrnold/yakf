#' @include utilities.R
#' @include KalmanFilter-class.R
#' @exportClass KalmanFilterBatch
NULL

#' @docType class
#' @aliases KalmanFilterBatch-class
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
#' \item{\code{a}}{\code{"list"} of \code{"Matrix"} obects, each of dimension
#' m x 1 containing the mean of predicted states.}
#' \item{\code{P}}{\code{"list"} of \code{"Matrix"} obects, each of dimension
#' \item{\code{a_filter}}{\code{"list"} of \code{"Matrix"} obects, each of dimension
#' m x 1 containing the mean of predicted states.}
#' \item{\code{P_filter}}{\code{"list"} of \code{"Matrix"} obects, each of dimension
#' m x m containing the covariance matrix for the predicted states.}
#' \item{\code{loglik}}{\code{"numeric"} vector of the log-likelihood for
#' each observation.}
#' }
#' 
#' @seealso \code{\link{kalman_filter}}, which returns objects of this class. \code{\linkS4class{KalmanFilterSeq}}
setClass("KalmanFilterBatch",
         representation(v = "Matrix",
                        K = "list",
                        Finv = "list",
                        a = "list",
                        P = "list",
                        a_filter = "ListOrNULL",
                        P_filter = "ListOrNULL",
                        loglik = "numeric"),
         contains = "KalmanFilter")

validity.KalmanFilterBatch <- function(object) {
  N <- nrow(object@v) # variables
  n <- ncol(object@v) # obs
  m <- nrow(object@K[[1]]) # states

  for (i in c("K", "Finv", "loglik", "a_filter", "P_filter")) {
    if (!is.null(slot(object, i)) && length(slot(object, i)) != n) {
      return(sprintf("ncol(%s) != %d = ncol(v)", i, n))
    }
  }
  for (i in c("a", "P")) {
    if (length(slot(object, i)) != (n + 1L)) {
      return(sprintf("ncol(%s) != %d = ncol(v) + 1", i, n + 1L))
    }
  }
  
  invalid <-
    list(K = function(x) !(is(x, "Matrix") && ncol(x) == n && nrow(x) == m),
         Finv = function(x) !(is(x, "Matrix") && ncol(x) == n && nrow(x) == n),
         a = function(x) !(is(x, "Matrix") && ncol(x) == 1L && nrow(x) == m),
         P =function(x) (is(x, "Matrix") && ncol(x) == m && nrow(x) == m),
         a_filter = function(x) !(is(x, "Matrix") && ncol(x) == 1L && nrow(x) == m),
         P_filter =function(x) (is(x, "Matrix") && ncol(x) == m && nrow(x) == m))


  for (i in names(invalid)) {
    if (any(sapply(slot(object, i), invalid[[i]]))) {
      return("invalid elements in the list in %s slot", sQuote(i))
    }
  }
  TRUE
}
