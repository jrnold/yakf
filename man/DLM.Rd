\name{DLM}
\alias{DLM}
\title{Create DLM objects}
\usage{
  DLM(T, Z, HH, GG, a1 = NULL, P1 = NULL, cc = NULL,
    dd = NULL, HG = NULL, tv_T = NULL, tv_HH = NULL,
    tv_Z = NULL, tv_GG = NULL, tv_cc = NULL, tv_dd = NULL,
    tv_HG = NULL, X = NULL)
}
\arguments{
  \item{T}{\code{matrix} of dimension m x m.}

  \item{Z}{\code{matrix} of dimension N x m.}

  \item{HH}{\code{matrix} of dimension m x m.}

  \item{GG}{\code{matrix} of dimension N x N.}

  \item{P1}{\code{matrix} of dimension m x m. If
  \code{NULL}, then a diffuse diagonal matrix is used. The
  diagonal of this matrix is set to 10^6 times the maximum
  of 1 or values along the diagonal of \code{HH} and
  \code{GG}.}

  \item{a1}{\code{numeric} of length m. If \code{NULL},
  then set to a 0 vector.}

  \item{dd}{\code{numeric} of dimension N. If \code{NULL},
  then set to a 0 vector.}

  \item{cc}{\code{numeric} of dimension m. If \code{NULL},
  then set to a 0 vector.}

  \item{HG}{\code{matrix} of dimension m x N. If
  \code{NULL}, then set to a 0 matrix.}

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
  This is a utility function to create objects of class
  \code{\linkS4class{DLM}}.
}
\seealso{
  \code{\linkS4class{DLM}}

  Other create.dlm: \code{\link{dlm_reg}}
}

