\name{dlm_seasonal}
\alias{dlm_seasonal}
\title{Create DLM for seasonal model}
\usage{
  dlm_seasonal(s, sigma = 1, GG = 1, a1 = NULL, P1 = NULL,
    cc = NULL, dd = NULL)
}
\arguments{
  \item{s}{Number of seasons}

  \item{sigma}{Standard deviation for the seasonal state.}
}
\value{
  An object of class \code{\linkS4class{DLM}}.
}
\description{
  Create DLM for seasonal model
}
\examples{
# DLM with 4 seasons
dlm_seasonal(4)
}
\seealso{
  Other create-DLM: \code{\link{dlm_arma}},
  \code{\link{dlm_polynomial}},
  \code{\link{dlm_recursion}}, \code{\link{dlm_reg}},
  \code{\link{dlm_spline}}
}

