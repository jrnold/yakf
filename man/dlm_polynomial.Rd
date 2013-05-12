\name{dlm_polynomial}
\alias{dlm_polynomial}
\title{n-th order polynomial DLM}
\usage{
  dlm_polynomial(order = 1, GG = 1, HH = NULL, a1 = NULL,
    P1 = NULL, cc = NULL, dd = NULL, HG = NULL)
}
\arguments{
  \item{order}{\code{integer} Order of the polynomial
  model. \code{order = 1} is a local level model,
  \code{order = 2} is a local trend model.}

  \item{GG}{\code{numeric} Observation variance, \eqn{G G'
  = \sigma^2}.}
}
\value{
  An object of class \code{\linkS4class{DLM}}.
}
\description{
  The function creates an \code{\linkS4class{DLM}} object
  with an n-th order polynomial.
}
\examples{
# local level model
dlm_polynomial(1, GG = 2)
# local trend model
dlm_polynomial(2, GG = 2)
# higer-order polynomial
dlm_polynomial(order = 5, GG = 3)
}

