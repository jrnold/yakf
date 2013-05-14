\docType{class}
\name{DLM}
\alias{+,DLM,DLM-method}
\alias{DLM}
\alias{DLM-class}
\title{DLM class}
\usage{
  DLM(T, Z, HH, GG, a1 = NULL, P1 = NULL, cc = NULL,
    dd = NULL, HG = NULL, tv_T = NULL, tv_HH = NULL,
    tv_Z = NULL, tv_GG = NULL, tv_cc = NULL, tv_dd = NULL,
    tv_HG = NULL, X = NULL)
}
\arguments{
  \item{T}{\code{Matrix} of dimension m x m.}

  \item{Z}{\code{Matrix} of dimension N x m.}

  \item{HH}{\code{Matrix} of dimension m x m.}

  \item{GG}{\code{Matrix} of dimension N x N.}

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

  \item{HG}{\code{Matrix} of dimension m x N. If
  \code{NULL}, then it set to a 0 matrix.}

  \item{tv_T}{\code{matrix} Indices of time-varying
  elements in \code{T}. See details.}

  \item{tv_Z}{\code{matrix} Indices of time-varying
  elements in \code{Z}. See details.}

  \item{tv_HH}{\code{matrix} Indices of time-varying
  elements in \code{HH}. See details.}

  \item{tv_GG}{\code{matrix} Indices of time-varying
  elements in \code{GG}. See details.}

  \item{tv_cc}{\code{matrix} Indices of time-varying
  elements in \code{cc}. See details.}

  \item{tv_dd}{\code{matrix} Indices of time-varying
  elements in \code{dd}. See details.}

  \item{tv_HG}{\code{matrix} Indices of time-varying
  elements in \code{HG}. See details.}

  \item{X}{\code{matrix} containing data used in
  time-varying parameters.}
}
\value{
  An object of class \code{\linkS4class{DLM}}.
}
\description{
  The \code{"DLM"} class represents a dynamic linear model,
  also called state-space model.
}
\section{Slots}{
  \describe{ \item{\code{T}}{\code{"Matrix"}. The \eqn{T}
  matrix.} \item{\code{Z}}{\code{"Matrix"}. The \eqn{Z}
  matrix.} \item{\code{HH}}{\code{"Matrix"}. The \eqn{H H'}
  matrix.} \item{\code{GG}}{\code{"Matrix"}. The \eqn{G G'}
  matrix.} \item{\code{P1}}{\code{"Matrix"}. The \eqn{P_1}
  matrix with the covariance of the initial states.}
  \item{\code{a1}}{\code{"Matrix"}. The \eqn{a_1} vector
  with the mean of initial states.}
  \item{\code{dd}}{\code{"Matrix"}. The \eqn{d} vector.}
  \item{\code{cc}}{\code{"Matrix"}. The \eqn{c} vector.}
  \item{\code{HG}}{\code{"Matrix"}. The \eqn{H G'} matrix.}
  \item{\code{tv_T}}{\code{"matrix"}. Time varying indices
  for the \code{T} slot. See details.}
  \item{\code{tv_Z}}{\code{"matrix"}. Time varying indices
  for the \code{Z} slot. See details.}
  \item{\code{tv_HH}}{\code{"matrix"}. Time varying indices
  for the \code{HH} slot. See details.}
  \item{\code{tv_GG}}{\code{"matrix"}. Time varying indices
  for the \code{GG} slot. See details.}
  \item{\code{tv_cc}}{\code{"matrix"}. Time varying indices
  for the \code{cc} slot. See details.}
  \item{\code{tv_dd}}{\code{"matrix"}. Time varying indices
  for the \code{dd} slot. See details.}
  \item{\code{tv_HG}}{\code{"matrix"}. Time varying indices
  for the \code{HG} slot. See details.}
  \item{\code{X}}{\code{"Matrix"}. Contains the
  time-varying parameters data.} }
}

\section{Details}{
  The structure and notation of DLM models in this class is
  based on that used in SsfPack and \pkg{dlm}. See the
  package vignette for details.
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
  \code{\linkS4class{DLM}}
}

