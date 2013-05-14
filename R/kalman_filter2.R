#' @include KalmanFilter-class.R
#' @export kalman_filter2
NULL

ll <- function(Finv, v) {
  observed <- !is.na(v)
  Nobs <- sum(observed)
  v[!observed] <- 0
  if (sum(observed) == 0) {
    0
  } else {
    as.numeric(- 0.5 * (Nobs * log(2 * base::pi) + log(1 / det(Finv)) + t(v) %*% Finv %*% v))
  }
}

#' Kalman Filter (Batch)
#'
#' Runs a Kalman Filter, updating the filter using all observations for a given time at once.
#'
#' @param object A \code{DLM} object
#' @param y \code{matrix} in which the rows are observations, and the columns
#' are variables.
#' @param likeonly If \code{TRUE}, then return only the log-likelihood.
#' @return If \code{likeonly = FALSE}, an object of class \code{KalmanFilter}. This
#' can be quite large, containing (N + N^2 + N * m + m + m^2 + 1) * n numbers (plus the class
#' overhead.  If \code{likeonly = TRUE}, then a \code{numeric} object with length 1,
#' containing the total log-likelihood.
#' @examples
#' # A local level model of the nile data
#' data(Nile)
#' mod <- dlm_polynomial(order = 1, HH = 1469, GG = 15099)
#' # predicted states
#' filter1 <- kalman_filter(mod, as.numeric(Nile))
#' # only the log-likelihood
#' kalman_filter(mod, as.numeric(Nile), likeonly = TRUE)
#' # The same model with missing values
#' nilem <- as.numeric(Nile)
#' nilem[c(2, 10, 45)] <- NA
#' filter2 <- kalman_filter(mod, nilem)
kalman_filter2 <- function(object, y, likeonly = FALSE) {
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
  if (! likeonly) {
    v_save <- Matrix(0, N, n, sparse=FALSE)
    K_save <- vector(n, mode = "list")
    Finv_save <- vector(n, mode = "list")
    a_filter_save <- vector(n, mode = "list")
    P_filter_save <- vector(n, mode = "list")
    a_save <- vector(n, mode = "list")
    P_save <- vector(n, mode = "list")
  }
  ll_save <- numeric(n)

  ## Allocate initial states
  # a and P are the forecast values
  a <- object@a1
  P <- object@P1
  if(! likeonly) {
    a_save[[1]] <- a
    P_save[[1]] <- P
  }
  for (i in 1:n) {
    yi <- y[i, ]
    cc <- dlm_cc(object, i)
    dd <- dlm_dd(object, i)
    T <- dlm_T(object, i)
    Z <- dlm_Z(object, i)
    HH <- dlm_HH(object, i)
    GG <- dlm_GG(object, i)

    is_obs <- ! is.na(yi)
    n_obs <- sum(is_obs)
    if (n_obs == 0) {
      a <- dd + T %*% a
      P <- T %*% P %*% t(T) + tcrossprod(HH)
      v <- rep(NA_real_, N)
      K <- Matrix(0, N, m)
      Finv <- Matrix(0, N, N)
    } else {
      # if any missing, adjust matrices
      if (n_obs < N) {
        yi <- yi[is_obs]
        touse <- c(is_obs)
        cc <- cc[touse, ]
        ZZ <- ZZ[touse, ]
        HH <- HH[touse, touse]
      }
      ## Filtering
      v <- yi - cc - Z %*% a;
      F <- Z %*% P %*% Z + HH
      Finv <- solve(F)
      M <- P %*% Z
      a <- a + M %*% Finv %*% v
      P <- symmetrize(P - M %*% Finv %*% t(M))
      if (! likeonly) {
        a_filter_save[[i]] <- a
        P_filter_save[[i]] <- P
      }
      ## Prediction
      a <- dd + T %*% a
      P <- symmetrize(T %*% P %*% t(T) + GG)

      ## Save results
      if (n_obs < N) {
        v_tmp <- rep(NA_real_, N)
        v_tmp[is_obs] <- v
        v <- v_tmp
        Finv_tmp <- Matrix(0, N, N, sparse=FALSE)
        Finv_tmp[is_obs, is_obs] <- Finv
        Finv <- Finv_tmp
        K_tmp <- Matrix(0, m, N, sparse=FALSE)
        K_tmp[ , is_obs] <- K
        K <- K_tmp
      }
    }
    
    ll_save[i] <- ll(Finv, v)
    if (! likeonly) {
      v_save[ , i] <- v
      Finv_save[[i]] <- Finv
      K_save[[i]] <- K
      a_save[[i + 1]] <- a
      P_save[[i + 1]] <- P
    }
  }

  if (! likeonly) {
    new("KalmanFilter",
        v = v_save,  # N
        Finv = Finv_save, # N^2
        K = K_save, # N m
        a = a_save, # m
        P = P_save, # m m
        a_filter = a_filter_save,
        P_filter = P_filter_save,
        loglik = ll_save
        )    # (N + N^2 + N m + m + m^2 + 1) * n
  } else {
    sum(ll_save)
  }
}
