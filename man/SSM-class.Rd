\docType{class}
\name{SSM}
\alias{+,SSM,SSM-method}
\alias{SSM}
\alias{SSM-class}
\title{SSM class}
\usage{
  SSM(T, Z, H, Q, R = NULL, a1 = NULL, P1 = NULL,
    cc = NULL, dd = NULL, kappa = 1e+07)
}
\arguments{
  \item{T}{\code{Matrix} of dimension m x m.}

  \item{Z}{\code{Matrix} of dimension p x m.}

  \item{H}{\code{Matrix} of dimension m x m.}

  \item{R}{\code{Matrix} of dimension p x r.}

  \item{Q}{\code{Matrix} of dimension r x r.}

  \item{P1}{\code{Matrix} of dimension m x m. If
  \code{NULL}, then a diffuse diagonal Matrix is used. The
  diagonal of this Matrix is set to 10^6 times the maximum
  of 1 or values along the diagonal of \code{HH} and
  \code{GG}.}

  \item{a1}{\code{Matrix} of length m. If \code{NULL}, then
  set to a 0 vector.}

  \item{dd}{\code{Matrix} of dimension N. If \code{NULL},
  then set to a 0 vector.}

  \item{cc}{\code{Matrix} of dimension m. If \code{NULL},
  then set to a 0 vector.}

  \item{kappa}{Default multiplier for diagonal values of
  \code{P1}, if \code{P1 = NULL}.}
}
\value{
  An object of class \code{\linkS4class{SSM}}.
}
\description{
  The \code{"SSM"} class represents a dynamic linear model,
  also called state-space model.
}
\section{Slots}{
  \describe{ \item{\code{T}}{\code{"MatrixOrMatrixList"}.
  The \eqn{T} matrix.}
  \item{\code{Z}}{\code{"MatrixOrMatrixList"}. The \eqn{Z}
  matrix.} \item{\code{H}}{\code{"MatrixOrMatrixList"}. The
  \eqn{H} matrix.}
  \item{\code{Q}}{\code{"MatrixOrMatrixList"}. The \eqn{Q}
  matrix.} \item{\code{R}}{\code{"MatrixOrMatrixList"}. The
  \eqn{R} matrix.}
  \item{\code{P1}}{\code{"MatrixOrMatrixList"}. The
  \eqn{P_1} matrix with the covariance of the initial
  states.} \item{\code{a1}}{\code{"MatrixOrMatrixList"}.
  The \eqn{a_1} vector with the mean of initial states.}
  \item{\code{dd}}{\code{"MatrixOrMatrixList"}. The \eqn{d}
  vector.} \item{\code{cc}}{\code{"MatrixOrMatrixList"}.
  The \eqn{c} vector.} }
}

\section{Details}{
  The structure and notation of state spece models
  represented this class is based on that used in SsfPack
  and \pkg{dlm}. See the package vignette for details.
}
\references{
  Giovanni Petris (2010), An R Package for Dynamic Linear
  Models.

  Petris, Petrone, and Campagnoli, Dynamic Linear Models
  with R, Springer (2009).

  West and Harrison, Bayesian forecasting and dynamic
  models (2nd ed.), Springer (1997).
}
\seealso{
  \code{\linkS4class{SSM}}
}

