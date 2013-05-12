#' @include yakf-package.R
#' @include KalmanFilter-class.R
#' @export kalman_filter
NULL

#' Kalman Filter (Batch)
#'
#' Multivariate Kalman filter.
#'
#' @param object A \code{DLM} object
#' @param y \code{matrix} in which the rows are observations, and the columns
#' are variables.
#' @return An object of class \code{KalmanFilter}.
#' @seealso \code{\link{kalman_filter_seq}} for the sequential Kalman filter.
#' @examples
#' # A local level model of the nile data
#' data(Nile)
#' mod <- dlm_polynomial(order = 1, HH = 1469, GG = 15099)
#' filter1 <- kalman_filter(mod, as.numeric(Nile))
#' # The same model with missing values
#' nilem <- as.numeric(Nile)
#' nilem[c(2, 10, 45)] <- NA
#' filter2 <- kalman_filter(mod, nilem)
kalman_filter <- function(object, y) {
  y <- Matrix(y)
  # check that number of variables are consistent
  if (ncol(y) != dlm_vars(object)) {
    stop("ncol(y) != number of variables in object")
  }
  # check observation length
  if (nrow(object@X)) {
    if (nrow(object@X) != nrow(y)) {
      stop("nrow(object@X) != nrow(y)")
    }
  }
  ## States and Variables
  m <- dlm_states(object)
  N <- dlm_vars(object)
  n <- nrow(y)
  ## Objects to save
  v_save <- Matrix(0, N, n, sparse=FALSE)
  K_save <- vector(n, mode = "list")
  Finv_save <- vector(n, mode = "list")

  ## Allocate initial states
  # a and P are the forecast values
  a <- object@a1
  P <- object@P1
  for (i in 1:n) {
    yi <- y[i, ]
    delta <- dlm_delta(object, i)
    Phi <- dlm_Phi(object, i)
    Omega <- dlm_Omega(object, i)

    is_obs <- ! is.na(yi)
    if (! sum(is_obs)) {
      d <- delta[1:m, ]
      T <- Phi[1:m, ]
      H <- Omega[1:m, 1:m]
      a <- d + T %*% a
      P <- T %*% P %*% t(T) + tcrossprod(H)
      v <- rep(0, N)
      K <- Matrix(0, N, m)
      Finv <- Matrix(0, N, N)
    } else {
      # if any missing, adjust matrices
      if (sum(! is_obs)) {
        yi <- yi[is_obs]
        touse <- c(rep(TRUE, m), is_obs)
        delta <- delta[touse, ]
        Phi <- Phi[touse, ]
        Omega <- Omega[touse, touse]
      }
      ay <- delta + Phi %*% a
      abar <- ay[1:m]
      yhat <- ay[m + 1:N]
      PMMF <- Phi %*% P %*% t(Phi) + Omega
      Pbar <- PMMF[1:m, 1:m]
      M <- PMMF[m + 1:N, 1:m]
      F <- PMMF[m + 1:N, m + 1:N]
      Finv <- solve(F)
      K <- M %*% Finv
      ## update
      v <- t(y[i, ]) - yhat
      a <- abar + K %*% v
      P <- Pbar - K %*% t(M)
      P <- 0.5 * (P + t(P)) # symmetrize
    }
    ## save
    v_save[ , i] <- v
    Finv_save[[i]] <- Finv
    K_save[[i]] <- K
  }

  new("KalmanFilter",
      v = v_save, Finv = Finv_save, K = K_save)
}
