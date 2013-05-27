#' @include SsmSamples-class.R
#' @include utilities.R
#' @include SSM-class.R
#' @export ssm_samples
NULL


#' Generate Samples from a State Space Models
#'
#' Generate sample states and observations from a
#' \code{"\linkS4class{SSM}"} object.
#'
#' @param object \code{SSM} object which specifies the state space model from
#' which the samples will be drawn.
#' @param n \code{numeric} Number of samples to draw. Only needed, and used,
#' if \code{SSM} is time-invariant.
#' \code{object} must have time-varying parameters.
#' @param a1 \code{numeric} Initial state. If missing, a random
#' initial state is drawn from the distribution of the initial state
#' in \code{object}.
#' @return An object of class \code{\linkS4class{SsmSamples}} containing
#' the sampled states and observations.
#' @seealso \code{\linkS4class{SsmSamples}}, \code{\linkS4class{SSM|}}
ssm_samples <- function(object, n = NULL, a1 = NULL) {
  ssm_dim <- dim(object)
  m <- ssm_dim['m']
  p <- ssm_dim['p']
  ssm_n <- ssm_dim['n']

  # Use whether n is specified to determine if it is time varying
  if (ssm_n) {
    if (! is.null(n)) {
      warning("object is not time-invariant; n is ignored.")
    }
    n <- ssm_n
  } else {
    if (is.null(n)) {
      stop("If object is a time-invariante SSM, then n must be specified")
    }
  }

  # Save
  a <- Matrix(0, m, n + 1, sparse=FALSE)
  y <- Matrix(0, p, n, sparse=FALSE)

  ## Copmutationally intensive things that can be done outside
  ## loop
  ## 1. draw random normal numbers
  ## 2. cholesky decomp of covariance matrices

  ## Normal random variates
  u1 <- Matrix(rnorm(n * p), p, n)
  u2 <- Matrix(rnorm(n * m), m, n)
  if (is.null(a1)) {
    a[ , 1] <- object@a1 + chol(object@P1) %*% rnorm(m)
  } else {
    if (length(a1) != length(object@a1)) {
      stop(sprintf("length(a1) != %d", m))
    }
    a[ , 1] <- a1
  }

  tv <- is_tv_matrix(object)
  if (! tv['T']) T <- object@T
  if (! tv['Z']) Z <- object@Z
  if (! tv['H']) {
    H_L <- chol(object@H)
  } else {
    Q_L_tv <- lapply(object@H, chol)    
  }
  if (! tv['Q']) {
    Q_L <- chol(object@Q)
  } else {
    Q_L_tv <- lapply(object@Q, chol)
  }
  if (! tv['R']) R <- object@R
  if (! tv['cc']) cc <- object@cc
  if (! tv['dd']) dd <- object@dd

  for (i in 1:n) {
    if (tv['T']) T <- object@T[[i]]
    if (tv['Z']) Z <- object@Z[[i]]
    if (tv['H']) {
      H_L <- H_L_tv[[i]]
    }
    if (tv['Q']) {
      Q_L <- Q_L_tv[[i]]
    }
    if (tv['R']) R <- object@R[[i]]
    if (tv['cc']) cc <- object@cc[[i]]
    if (tv['dd']) dd <- object@dd[[i]]

    a[ , i + 1] <-
      (dd + T %*% a[ , i]
       + R %*% Q_L %*% u2[ , i, drop=FALSE])
    y[ , i] <- cc + Z %*% a[ , i + 1] + H_L %*% u1[ , i, drop=FALSE]
  }
  SsmSamples(a = a, y = y)
}
