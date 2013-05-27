\name{ssm_poly}
\alias{ssm_poly}
\title{n-th order polynomial SSM}
\usage{
  ssm_poly(order = 1L, H = 1, Q = NULL, ...)
}
\arguments{
  \item{order}{\code{integer} Order of the polynomial
  model. \code{order = 1} is a local level model,
  \code{order = 2} is a local trend model.}

  \item{GG}{\code{numeric} Observation variance, \eqn{G G'
  = \sigma^2}.}
}
\value{
  An object of class \code{\linkS4class{SSM}}.
}
\description{
  The function creates an \code{\linkS4class{SSM}} object
  with an n-th order polynomial.
}
\examples{
# local level model
ssm_poly(1, GG = 2)
# local trend model
ssm_poly(2, GG = 2)
# higer-order polynomial
ssm_poly(order = 5, GG = 3)
}
\seealso{
  Other create.SSM: \code{\link{ssm_reg}}
}

