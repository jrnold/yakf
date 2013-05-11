\docType{class}
\name{DLM}
\alias{DLM}
\alias{DLM-class}
\title{DLM class}
\description{
  The \code{"DLM"} class represents a dynamic linear model,
  also called state-space model.
}
\section{Slots}{
  \describe{ \item{\code{T}}{\code{"matrix"}. The \eqn{T}
  matrix.} \item{\code{Z}}{\code{"matrix"}. The \eqn{Z}
  matrix.} \item{\code{GG}}{\code{"matrix"}. The \eqn{G G'}
  matrix.} \item{\code{HH}}{\code{"matrix"}. The \eqn{H H'}
  matrix.} \item{\code{a1}}{\code{"matrix"}. The \eqn{a_1}
  vector with the mean of initial states.}
  \item{\code{P1}}{\code{"matrix"}. The \eqn{P_1} matrix
  with the covariance of the initial states.}
  \item{\code{cc}}{\code{"numeric"}. The \eqn{c} vector.}
  \item{\code{dd}}{\code{"numeric"}. The \eqn{d} vector.}
  \item{\code{HG}}{\code{"matrix"}. The \eqn{H G'} matrix.}
  \item{\code{tv_GG}}{\code{"matrix"}. Time varying indices
  for the \code{GG} slot. See details.}
  \item{\code{tv_HH}}{\code{"matrix"}. Time varying indices
  for the \code{HH} slot. See details.}
  \item{\code{tv_HG}}{\code{"matrix"}. Time varying indices
  for the \code{HG} slot. See details.}
  \item{\code{tv_T}}{\code{"matrix"}. Time varying indices
  for the \code{T} slot. See details.}
  \item{\code{tv_Z}}{\code{"matrix"}. Time varying indices
  for the \code{Z} slot. See details.}
  \item{\code{tv_cc}}{\code{"integer"}. Time varying
  indices for the \code{cc} slot. See details.}
  \item{\code{tv_dd}}{\code{"integer"}. Time varying
  indices for the \code{dd} slot. See details.}
  \item{\code{X}}{\code{"matrix"}. Contains the
  time-varying parameters data.} }
}

\section{Details}{
  The internal structure of this class is based on that
  used in SsfPack and \pkg{dlm}. See the package vignette
  for details.
}

