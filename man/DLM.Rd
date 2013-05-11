\name{DLM}
\alias{DLM}
\title{Create DLM objects}
\usage{
  DLM(T, HH, Z, GG, a1 = NULL, P1 = NULL, cc = NULL,
    dd = NULL, HG = NULL, tv_T = NULL, tv_HH = NULL,
    tv_Z = NULL, tv_GG = NULL, tv_cc = NULL, tv_dd = NULL,
    tv_HG = NULL, X = NULL)
}
\arguments{
  \item{T}{\code{matrix} of dimension m x m.}

  \item{HH}{\code{matrix} of dimension m x m.}

  \item{Z}{\code{matrix} of dimension N x m.}

  \item{GG}{\code{matrix} of dimension N x N.}

  \item{a1}{\code{numeric} of length m.}

  \item{P1}{\code{matrix} of dimension m x m.}

  \item{cc}{\code{numeric} of dimension m.}

  \item{dd}{\code{numeric} of dimension N.}

  \item{HG}{\code{matrix} of dimension m x N.}

  \item{tv_T}{\code{matrix} of same dimension as \code{T}.}

  \item{tv_HH}{\code{matrix} of same dimension as
  \code{HH}.}

  \item{tv_Z}{\code{matrix} of same dimension as \code{Z}.}

  \item{tv_GG}{\code{matrix} of same dimension as
  \code{GG}.}

  \item{tv_cc}{\code{matrix} of same dimension as
  \code{cc}.}

  \item{tv_dd}{\code{matrix} of same dimension as
  \code{dd}.}

  \item{tv_HG}{\code{matrix} of same dimension as
  \code{HG}.}

  \item{X}{\code{matrix} containing time varying data.}
}
\value{
  An object of class \code{\linkS4class{DLM}}.
}
\description{
  This is a utility function to create objects of class
  \code{\linkS4class{DLM}}.
}
\seealso{
  \code{\linkS4class{DLM}} for a description of DLM
  objects.
}

