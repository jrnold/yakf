\name{kalman_filter}
\alias{kalman_filter}
\title{Kalman Filter (Batch)}
\usage{
  kalman_filter(object, y)
}
\arguments{
  \item{object}{A \code{DLM} object}

  \item{y}{\code{matrix} in which the rows are
  observations, and the columns are variables.}
}
\value{
  An object of class \code{KalmanFilter}.
}
\description{
  Multivariate Kalman filter.
}
\examples{
# A local level model of the nile data
data(Nile)
mod <- dlm_polynomial(order = 1, HH = 1469, GG = 15099)
filter1 <- kalman_filter(mod, as.numeric(Nile))
# The same model with missing values
nilem <- as.numeric(Nile)
nilem[c(2, 10, 45)] <- NA
filter2 <- kalman_filter(mod, nilem)
}
\seealso{
  \code{\link{kalman_filter_seq}} for the sequential Kalman
  filter.
}

