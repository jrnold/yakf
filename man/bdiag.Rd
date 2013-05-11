\name{bdiag}
\alias{bdiag}
\title{Create Block Diagonal Matrix}
\usage{
  bdiag(..., offdiag = 0)
}
\arguments{
  \item{...}{\code{matrix} object to form the diagonal
  blocks.}

  \item{offdiag}{value used to fill in the off-diagonal
  elements.}
}
\value{
  An object of class \code{matrix} with the matrices from
  \code{...} along the diagonal, and \code{offdiag} for the
  entries off the block diagonal.
}
\description{
  Create Block Diagonal Matrix
}

