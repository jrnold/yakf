\docType{class}
\name{SsmSamples}
\alias{SsmSamples}
\alias{SsmSamples-class}
\title{Object containing state space model samples}
\usage{
  SsmSamples(...)
}
\arguments{
  \item{...}{Data to include in the new object.}
}
\description{
  Samples of observations and states generate from a state
  space model. These objects are usually generated by
  \code{\link{ssm_recursion}}.

  The function \code{SsmSamples} is a generator function
  for objects of this class.
}
\section{Slots}{
  \describe{ \item{\code{a}}{\code{Matrix} containing the
  latent states} \item{\code{y}}{\code{Matrix} containing
  the observations} }
}
\seealso{
  \code{\link{ssm_samples}} which returns object of this
  class.
}

