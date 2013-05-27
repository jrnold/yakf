\name{ssm_reg}
\alias{ssm_reg}
\title{Create SSM object for a TVP regression model}
\usage{
  ssm_reg(X, intercept = TRUE, H = 1, Q = NULL, ...)
}
\arguments{
  \item{X}{\code{matrix} The design matrix.}

  \item{GG}{\code{numeric} The observation variance.}

  \item{HH}{\code{numeric} The system variance matrix. This
  should have be a square matrix of dimensions
  \code{ncol(X) + 1} if \code{intercept = TRUE}, otherwise
  of dimension \code{ncol(X)}.}

  \item{intercept}{\code{logical} Include an intercept?}
}
\value{
  An object of class \code{SSM}
}
\description{
  This function creates a \code{"SSM"} object with a
  representation of a lienar regression model with
  time-varying parameters.
}
\examples{
x <- matrix(rnorm(6), ncol = 2)
ssm_reg(x)
}
\seealso{
  Other create.SSM: \code{\link{ssm_poly}}
}

