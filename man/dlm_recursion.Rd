\name{dlm_recursion}
\alias{dlm_recursion}
\title{Generate Samples from a DLM}
\usage{
  dlm_recursion(object, n = NULL, a1 = NULL)
}
\arguments{
  \item{object}{\code{DLM}}

  \item{n}{\code{numeric} Number of samples to draw. If
  NULL, then \code{object} must have time-varying
  parameters.}

  \item{a1}{\code{numeric} Initial state. If missing, a
  random initial state is drawn from the distribution of
  the initial state in \code{object}.}
}
\value{
  An object of class \code{\linkS4class{DlmSamples}}
  containing the sampled states and observations.
}
\description{
  Generate sample states and observations from a specified
  \code{"\linkS4class{DLM}"} object.
}
\seealso{
  \code{\linkS4class{DlmSamples}} # sample from a local
  level with signal to noise ratio of 0.5 mod <-
  dlm_polynomial(1) dlm_recursion(mod, n = 100, a1 =
  matrix(0))

  Other create-DLM: \code{\link{dlm_arma}},
  \code{\link{dlm_polynomial}}, \code{\link{dlm_reg}},
  \code{\link{dlm_seasonal}}, \code{\link{dlm_spline}}
}

