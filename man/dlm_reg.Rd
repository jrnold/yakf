\name{dlm_reg}
\alias{dlm_reg}
\title{Create DLM object for a TVP regression model}
\usage{
  dlm_reg(X, intercept = TRUE, GG = 1, HH = NULL,
    cc = NULL, dd = NULL, a1 = NULL, P1 = NULL)
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
  An object of class \code{DLM}
}
\description{
  This function creates a \code{"DLM"} object with a
  representation of a lienar regression model with
  time-varying parameters.
}
\examples{
x <- matrix(rnorm(6), ncol = 2)
dlm_reg(x)
}
\seealso{
  Other create.dlm: \code{\link{DLM}}
}

