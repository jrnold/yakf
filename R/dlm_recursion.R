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
#' # sample from a local level with signal to noise ratio of 0.5
#' mod <- dlm_polynomial(1)
#' dlm_recursion(mod, n = 100, a1 = matrix(0))
dlm_recursion <- function(object, n = NULL, a1 = NULL) {
  m <- dlm_states(object)
  N <- dlm_vars(object)

  # Use whether n is specified to determine if it is time varying
  if (is.null(n)) {
    istv <- TRUE
    if (!(dlm_obs(object))) {
      stop("if n = NULL, then object must have time-varying parameters")
    }
  } else {
    istv <- FALSE
  }

  # Save
  a <- Matrix(0, m, n + 1, sparse=FALSE)
  y <- Matrix(0, N, n, sparse=FALSE)

  ## Normal random variates
  u <- Matrix(rnorm(n * (m + N)), m + N, n)

  if (is.null(a1)) {
    a[ , 1] <- object@a1 + chol(object@P1) %*% rnorm(m)
  } else {
    if (length(a1) != length(object@a1)) {
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
    ay <- delta + Phi %*% a[ , i] + chol(Omega) %*% u[ , i]
    a[ , i + 1] <- ay[1:m]
    y[ , i] <- ay[m + 1:N]
  }
  DlmSamples(a = a, y = y)
}
