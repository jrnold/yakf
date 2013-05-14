#' @include KalmanFilterSeq-class.R
#' @export kalman_filter_seq
NULL

llsparse <- function(Finv, v) {
  -0.5 * (log(2 * base::pi) - log(Finv) + Finv * v^2)
}

#' Kalman Filter (Sequential)
#'
#' This function runs a Kalman filter, updating with each observation.
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
#' filter1 <- kalman_filter_seq(mod, as.numeric(Nile))
#' # only the log-likelihood
#' kalman_filter_seq(mod, as.numeric(Nile), likeonly = TRUE)
#' # The same model with missing values
#' nilem <- as.numeric(Nile)
#' nilem[c(2, 10, 45)] <- NA
#' filter2 <- kalman_filter_seq(mod, nilem)
kalman_filter_seq <- function(object, y, likeonly = FALSE) {

  
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
    a_save <- vector(n + 1, mode = "list")
    P_save <- vector(n + 1, mode = "list")
    a1_save <- vector(n, mode = "list")
    P1_save <- vector(n, mode = "list")
  }
  ## Check sparsity of y
  ntotal <- length(y)
  nmissing <- sum(is.na(y))
  usesparse <- (nmissing / ntotal) > 0.5
  ll_save <- Matrix(0, N, n, sparse = usesparse)

  ## Allocate initial states
  # a and P are the forecast values
  a <- object@a1
  P <- object@P1
  if(! likeonly) {
    a_save[[1]] <- a
    P_save[[1]] <- P
  }
  for (i in 1:n) {
    T <- dlm_T(object, i)
    dd <- dlm_dd(object, i)
    HH <- dlm_HH(object, i)

    if (! likeonly) {
      v_tmp <- Matrix(NA_real_, N, 1, sparse=FALSE)
      Finv_tmp <- Matrix(0, N, 1, sparse=FALSE)
      K_tmp <- Matrix(0, m, N, sparse=FALSE)
    }
    
    for (j in 1:N) {
      yij <- y[i, j]
      if (!is.na(yij)) {
        Z <- dlm_Z(object, i)[j, ]
        cc <- dlm_cc(object, i)[j, ]
        sigma2 <- dlm_GG(object, i)[j, j]
        
        ####### Filtering
        # v_{t,i} = y_{t,i} - Z_{t,i} a_{t,i}
        v <- yij - cc - Z %*% a
        # M_{t,i} = P_{t,i} Z_{t,i}'
        M <- P %*% t(Z)
        # F_{t,i} = Z_{t,i} M_{t,i} + \sigma^2_{t,i}
        # F^{-1}_{t,i} = 1 / F_{t,i}
        Finv <- 1 / (Z %*% M + sigma2)
        # K_{t,i} = M_{t,i} F^{-1}_{t,i}
        K <- M %*% Finv
        # a_{t,i+t} = a_{t,i} + K_{t,i} v_{t,i}
        a <- a + K %*% v
        # P_{t,i+1} = P_{t,i} - K_{t,i} M_{t,i}'
        P <- symmetrize(P - K %*% M)
        ll_save[j, i] <- llsparse(Finv, v)
        if (! likeonly) {
          v_tmp[j, 1] <- v
          Finv_tmp[j, 1] <- Finv
          K_tmp[ , j] <- K
        }
      } else {
        ll_save[j, i] <- 0.0
      }
    }
    # filtered states a_{t,N+1}, P_{t,N+1}
    if (! likeonly) {
      a1_save[[i]] <- a
      P1_save[[i]] <- P
    }
    
    ## Prediction
    ## Predicted states
    a <- dd + T %*% a
    P <- symmetrize(T %*% P %*% t(T) + HH)

    if (! likeonly) {
      v_save[ , i] <- v_tmp
      Finv_save[[i]] <- Finv_tmp
      K_save[[i]] <- K_tmp
      a_save[[i + 1]] <- a
      P_save[[i + 1]] <- P
    }
  }

  if (! likeonly) {
    new("KalmanFilterSeq",
        v = v_save,  # N
        Finv = Finv_save, # N^2
        K = K_save, # N m
        a = a_save, # m
        P = P_save, # m m
        a_filter = a1_save, # m
        P_filter = P1_save, # m m
        loglik = ll_save
        )
  } else {
    sum(ll_save)
  }
}
