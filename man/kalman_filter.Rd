\name{kalman_filter}
\alias{kalman_filter}
\title{Kalman Filter (Batch)}
\usage{
  kalman_filter(object, y, likeonly = FALSE)
}
\arguments{
  \item{object}{A \code{DLM} object}

  \item{y}{\code{matrix} in which the rows are
  observations, and the columns are variables.}

  \item{likeonly}{If \code{TRUE}, then return only the
  log-likelihood.}
}
\value{
  If \code{likeonly = FALSE}, an object of class
  \code{KalmanFilter}. This can be quite large, containing
  (N + N^2 + N * m + m + m^2 + 1) * n numbers (plus the
  class overhead.  If \code{likeonly = TRUE}, then a
  \code{numeric} object with length 1, containing the total
  log-likelihood.
}
\description{
  Runs a Kalman Filter, updating the filter using all
  observations for a given time at once.
}
\examples{
# A local level model of the nile data
data(Nile)
mod <- dlm_polynomial(order = 1, HH = 1469, GG = 15099)
# predicted states
filter1 <- kalman_filter(mod, as.numeric(Nile))
# only the log-likelihood
kalman_filter(mod, as.numeric(Nile), likeonly = TRUE)
# The same model with missing values
nilem <- as.numeric(Nile)
nilem[c(2, 10, 45)] <- NA
filter2 <- kalman_filter(mod, nilem)
}

