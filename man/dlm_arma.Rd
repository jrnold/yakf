\name{dlm_arma}
\alias{dlm_arma}
\title{Create DLM object for an ARMA model}
\usage{
  dlm_arma(ar = 1, ma = 0, sigma = 1, GG = 0, a1 = NULL,
    P1 = NULL)
}
\arguments{
  \item{ar}{AR Coefficients}

  \item{ma}{MA coefficients}

  \item{sigma}{Standard deviation of MA error.}
}
\value{
  An object of class \code{"\linkS4class{DLM}"}.
}
\description{
  Create DLM object for an ARMA model
}
\section{Details}{
  If both \code{a1} and \code{P1} are \code{NULL} then
  \code{a1} = 0, and \code{P1} is set to the steady state
  value of \code{P}.
}
\examples{
dlm_arma(c(0.6, 0.2), -0.2, sqrt(0.9))
}
\seealso{
  Other create.DLM: \code{\link{dlm_spline}}
}

