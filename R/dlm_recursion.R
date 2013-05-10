#' @include yakf-package.R
#' @include DLM-class.R
#' @export dlm_recursion
NULL

#' Generate Samples from a DLM
#'
#' Generate sample states and observations from a specified
#' \code{\linkS4class{"DLM"}} object.
#'
#' @param object \code{DLM}
#' @param n \code{numeric} Number of samples to draw
#' @param a1 \code{numeric} Initial state. If missing, a random
#' initial state is drawn from the distribution of the initial state
#' in \code{object}.
#' @return A \code{list} with elements
#' \describe{
#' \item{\code{a}}{\code{matrix} object with dimensions m x (n + 1)}
#' \item{\code{y}}{\code{matrix} object with dimensions N x n.
#' The first column is set to 0.}
#' }
dlm_recursion <- function(object, n, a1 = NULL) {
  m <- length(object@a1)
  N <- nrow(object@Z)

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
  ## - draw all normal RV and multiply by cholesky of Omega
  ## - check if TV, and only draw delta / Phi / Omega if TV
  for (i in 1:n) {
    delta <- dlm_delta(object, i)
    Phi <- dlm_phi(object, i)
    Omega <- dlm_Omega(object, i)
    ay <- as.numeric(rmvnorm(1, delta + Phi %*% a[ , i], Omega))
    a[ , i + 1] <- ay[1:m]
    y[ , i] <- ay[m + 1:N]
  }
  list(a = a, y = y)
}
