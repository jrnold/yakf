\name{dlm_cyclical}
\alias{dlm_cyclical}
\alias{dlm_cyclical_season}
\alias{dlm_cyclical_tau}
\title{Create Fourier representation of a cyclical DLM}
\usage{
  dlm_cyclical(omega, q = 1, GG = 1, HH = NULL, a1 = NULL,
    P1 = NULL, season = FALSE)

  dlm_cyclical_tau(tau, ...)

  dlm_cyclical_season(s, q = floor(s/2), season = TRUE,
    ...)
}
\arguments{
  \item{omega}{\code{numeric} The frequency.}

  \item{q}{\code{integer} The number of harmonics.}

  \item{HH}{\code{matrix} The system covariance matrix,
  \eqn{H H'}. If \code{NULL}, then \eqn{H H' = 0}.}

  \item{season}{\code{logical} If \code{TRUE}, then the
  period is assumed to be an integer. This results in a
  special case if the period is even and \code{q = s / 2}.}

  \item{...}{Arguments passed to \code{dlm_cyclical}.}

  \item{tau}{\code{numeric} The period, \eqn{\omega =
  \frac{2 \pi}{\tau}}{2 pi / tau}.}

  \item{s}{\code{integer} The number of seasons,
  \eqn{\omega = \frac{2 \pi}{s}}{omega = 2 p / s}.}
}
\value{
  An object of class \code{\linkS4class{DLM}}.
}
\description{
  This function creates a \code{\linkS4class{DLM}} object
  for with a Fourier representation of a cyclical model.
}

