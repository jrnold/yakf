\name{ssm_samples}
\alias{ssm_samples}
\title{Generate Samples from a State Space Models}
\usage{
  ssm_samples(object, n = NULL, a1 = NULL)
}
\arguments{
  \item{object}{\code{SSM} object which specifies the state
  space model from which the samples will be drawn.}

  \item{n}{\code{numeric} Number of samples to draw. Only
  needed, and used, if \code{SSM} is time-invariant.
  \code{object} must have time-varying parameters.}

  \item{a1}{\code{numeric} Initial state. If missing, a
  random initial state is drawn from the distribution of
  the initial state in \code{object}.}
}
\value{
  An object of class \code{\linkS4class{SsmSamples}}
  containing the sampled states and observations.
}
\description{
  Generate sample states and observations from a
  \code{"\linkS4class{SSM}"} object.
}
\seealso{
  \code{\linkS4class{SsmSamples}},
  \code{\linkS4class{SSM|}}
}

