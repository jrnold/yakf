\name{dlm_polynomial}
\alias{dlm_polynomial}
\title{n-th order polynomial DLM}
\usage{
  dlm_polynomial(HH, GG, order = 1, a1 = NULL, P1 = NULL,
    cc = NULL, dd = NULL, HG = NULL)
}
\arguments{
  \item{order}{Order of the polynomial model. \code{order =
  1} is a local level model, \code{order = 2} is a local
  trend model.}
}
\value{
  An object of class \code{\linkS4class{DLM}}.
}
\description{
  The function creates an \code{\linkS4class{DLM}} object
  with an n-th order polynomial.
}
\examples{
# local trend model
dlm_polynomial(diag(1, 2, 2), 1, order = 2)
# 5th order
dlm_polynomial(diag(1, 5, 5), 1, order = 5)
}

