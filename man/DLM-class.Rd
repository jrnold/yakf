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
  The internal structure of this class is based on that
  used in SsfPack and \pkg{dlm}. See the package vignette
  for details.
}

