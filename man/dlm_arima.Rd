\name{dlm_arima}
\alias{dlm_arima}
\title{Create DLM object for an ARMA model}
\usage{
  dlm_arima(ar = 1, ma = 0, d = 0, sigma = 1, GG = 0,
    a1 = NULL, P1 = NULL, cc = NULL, dd = NULL)
}
\arguments{
  \item{ar}{AR Coefficients}

  \item{ma}{MA coefficients}

  \item{sigma}{Standard deviation of MA error.}

  \item{...}{Arguments passed to \code{dlm_arima}.}
}
\value{
  An object of class \code{"\linkS4class{DLM}"}.
}
\description{
  \code{dlm_arima} creates state space representations of
  ARIMA models.
}
\section{Details}{
  If both \code{a1} and \code{P1} are \code{NULL} then
  \code{a1} = 0, and \code{P1} is set to the steady state
  value of \code{P}.
}
\examples{
# an ARMA(2, 1)
dlm_arima(c(0.6, 0.2), -0.2, sqrt(0.9))
# an ARIMA(2, 1, 1) model
dlm_arima(c(0.6, 0.2), -0.2, sqrt(0.9), d = 1)
}
\seealso{
  Other create-DLM: \code{\link{dlm_polynomial}},
  \code{\link{dlm_recursion}}, \code{\link{dlm_reg}},
  \code{\link{dlm_seasonal}}, \code{\link{dlm_spline}}
}

