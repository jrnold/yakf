\name{dlm-accessors}
\alias{dlm-accessors}
\alias{dlm_HG}
\alias{dlm_HH}
\alias{dlm_Omega}
\alias{dlm_Phi}
\alias{dlm_Sigma}
\alias{dlm_T}
\alias{dlm_Z}
\alias{dlm_cc}
\alias{dlm_dd}
\alias{dlm_delta}
\alias{dlm_obs}
\alias{dlm_states}
\alias{dlm_vars}
\title{Access data from DLM objects}
\arguments{
  \item{object}{An object of class \code{DLM}.}

  \item{i}{time}
}
\value{
  A \code{matrix} or \code{numeric} vector.
}
\description{
  These are some utility functions to access system
  matrices from a DLM object. They are particularly useful
  if the object has time varying parameters.
}
\section{Details}{
  \describe{ \item{\code{dlm_cc}}{Return \eqn{c_t} vector}
  \item{\code{dlm_dd}}{Return \eqn{d_t} vector}
  \item{\code{dlm_T}}{Return \eqn{T_t} matrix}
  \item{\code{dlm_Z}}{Return \eqn{Z_t} matrix}
  \item{\code{dlm_HH}}{Return \eqn{H_t H_t'} matrix}
  \item{\code{dlm_GG}}{Return \eqn{G_t G_t'} matrix}
  \item{\code{dlm_HG}}{Return \eqn{H_t G_t'} matrix}
  \item{\code{dlm_Omega}}{Return the matrix, \deqn{\Omega_t
  = ( H_t H_t' , H_t G_t' ; G_t H_t' , G_t G_t' )} }
  \item{\code{dlm_delta}}{Return the matrix \deqn{\delta_t
  = ( d_t ; c_t )}} \item{\code{dlm_Phi}}{Return the matrix
  \deqn{\Phi_t = ( T_t ; Z_t )}}
  \item{\code{dlm_Sigma}}{Return the matrix \deqn{\Sigma =
  (P_1, a_1' )}} \item{\code{dlm_states}}{Return the number
  of states, \eqn{m}} \item{\code{dlm_vars}}{Return the
  number of variables, \eqn{N}}
  \item{\code{dlm_obs}}{Return the number of observations;
  returns 0 if not time-varying} }
}

