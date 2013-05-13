\name{dlm_spline}
\alias{dlm_spline}
\title{Create DLM of a cubic spline smoother}
\usage{
  dlm_spline(q, delta = 1, a1 = NULL, P1 = NULL, cc = NULL,
    dd = NULL)
}
\arguments{
  \item{q}{Signal to noise ratio (smoothness penalty).}

  \item{delta}{distance between points}

  \item{a1}{passed to \code{\link{DLM}}.}

  \item{P1}{passed to \code{\link{DLM}}.}
}
\value{
  An object of class \code{"\linkS4class{DLM}"}.
}
\description{
  Create DLM of a cubic spline smoother
}
\examples{
# cubic spline with equally spaced observations
# and signal-to-noise ratio of 0.2
dlm_spline(0.2)
# cubic spline with non-equally spaced observations
dlm_spline(0.2, diff(c(1, 2, 3, 5, 9, 12, 17, 20, 23, 25)))
}
\seealso{
  Other create-DLM: \code{\link{dlm_arma}},
  \code{\link{dlm_polynomial}},
  \code{\link{dlm_recursion}}, \code{\link{dlm_reg}},
  \code{\link{dlm_seasonal}}
}

