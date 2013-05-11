#' @include yakf-package.R
#' @include DlmSamples-class.R
#' @export dlm_recursion
NULL

#' Generate Samples from a DLM
#'
#' Generate sample states and observations from a specified
#' \code{\linkS4class{"DLM"}} object.
#'
#' @param object \code{DLM}
#' @param n \code{numeric} Number of samples to draw. If NULL, then
#' \code{object} must have time-varying parameters.
#' @param a1 \code{numeric} Initial state. If missing, a random
#' initial state is drawn from the distribution of the initial state
#' in \code{object}.
#' @return An object of class \code{\linkS4class{DlmSamples}} containing
#' the sampled states and observations.
#' @seealso \code{\linkS4class{DlmSamples}}
dlm_recursion <- function(object, n = NULL, a1 = NULL) {
  m <- length(object@a1)
  N <- nrow(object@Z)

  # Use whether n is specified to determine if it is time varying
  if (is.null(n)) {
    istv <- TRUE
    n <- nrow(object@X)
    if (n == 0) {
      stop("if n = NULL, then object must have time-varying parameters")
    }
  } else {
    istv <- FALSE
  }

  # Save
  a <- matrix(0, m, n + 1)
  y <- matrix(0, N, n)

  if (is.null(a1)) {
    a[ , 1] <- rmvnorm(1, object@a1, object@P1)
  } else {
    if (length(a1) != object@a1) {
      stop(sprintf("length(a1) != %d", m))
    }
    a[ , 1] <- a1
  }
  ## TODO:
  ## Speed ups
  ## - check if TV, and only draw delta / Phi / Omega if TV
  if (! istv) {
    delta <- dlm_delta(object)
    Phi <- dlm_Phi(object)
    Omega <- dlm_Omega(object)
  }
  for (i in 1:n) {
    if (istv) {
      delta <- dlm_delta(object, i)
      Phi <- dlm_Phi(object, i)
      Omega <- dlm_Omega(object, i)
    }
    ay <- as.numeric(rmvnorm(1, delta + Phi %*% a[ , i], Omega))
    a[ , i + 1] <- ay[1:m]
    y[ , i] <- ay[m + 1:N]
  }
  DlmSamples(a = a, y = y)
}
