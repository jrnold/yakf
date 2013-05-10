\name{dlm_recursion}
\alias{dlm_recursion}
\title{Generate Samples from a DLM}
\usage{
  dlm_recursion(object, n, a1 = NULL)
}
\arguments{
  \item{object}{\code{DLM}}

  \item{n}{\code{numeric} Number of samples to draw}

  \item{a1}{\code{numeric} Initial state. If missing, a
  random initial state is drawn from the distribution of
  the initial state in \code{object}.}
}
\value{
  A \code{list} with elements \describe{
  \item{\code{a}}{\code{matrix} object with dimensions m x
  (n + 1)} \item{\code{y}}{\code{matrix} object with
  dimensions N x n. The first column is set to 0.} }
}
\description{
  Generate sample states and observations from a specified
  \code{\linkS4class{"DLM"}} object.
}

