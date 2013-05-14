\docType{class}
\name{KalmanFilterBatch}
\alias{KalmanFilterBatch}
\alias{KalmanFilterBatch-class}
\title{Kalman filter results}
\description{
  Class containing results from a Kalman filter, as
  produced by (\code{\link{kalman_filter}}).
}
\section{Slots}{
  \describe{

  \item{\code{v}}{\code{"Matrix"} of size N x n. The
  forecast errors.}

  \item{\code{K}}{\code{"list"} of \code{"Matrix"} obects,
  each of dimension N x m containing the Kalman gain for
  each observation.}

  \item{\code{Finv}}{\code{"list"} of \code{"Matrix"}
  obects, each of dimension N x N containing the
  \eqn{F^{-1}} matrices.}

  \item{\code{a}}{\code{"list"} of \code{"Matrix"} obects,
  each of dimension m x 1 containing the mean of predicted
  states.}

  \item{\code{P}}{\code{"list"} of \code{"Matrix"} obects,
  each of dimension m x 1 containing the mean of predicted
  states.}

  \item{\code{a_filter}}{\code{"list"} of \code{"Matrix"}
  obects, each of dimension m x 1 containing the mean of
  filtered states.}

  \item{\code{P_filter}}{\code{"list"} of \code{"Matrix"}
  obects, each of dimension m x m containing the covariance
  matrix for the filtered states.}

  \item{\code{loglik}}{\code{"numeric"} vector of the
  log-likelihood for each observation.}

  }
}
\seealso{
  \code{\link{kalman_filter}}, which returns objects of
  this class. \code{\linkS4class{KalmanFilterSeq}}.
}

