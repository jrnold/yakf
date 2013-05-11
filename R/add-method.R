#' @include DLM-class.R
#' @exportMethod +
NULL

add.DLM <- function(e1, e2) {
  if (nrow(e1@Z) != nrow(e1@Z)) {
    stop("Incompatible DLM objects. object@Z must have same number of rows in each.")
  }
  if (nrow(e1@X) > 0
      && nrow(e2@X) > 0
      && nrow(e1@X) != nrow(e2@X)) {
    stop("Incompatible DLM objects. number of rows in X matrices must match.")
  }
  # observation
  cc <- e1@cc + e2@cc
  T <- cbind(e1@T, e2@T)
  HH <- bdiag(e1@HH, e2@HH)
  # system
  dd <- c(e1@dd, e2@dd)
  Z <- bdiag(e1@Z, e2@Z)
  GG <- bdiag(e1@GG, e2@GG)
  # covariance
  HG <- bdiag(e1@HG, e2@HG)
  # initial
  a1 <- c(e1@a1, e2@a1)
  P1 <- bdiag(e1@P1, e2@P1)
  # TODO: Time varying parameters
  X <- cbind(e1@X, e2@X)
  ## offset indices from e2.
  xoffset <- ncol(e1@X)
  tv_T <- bdiag(e1@tv_T, e2@tv_T + xoffset, offdiag = NA_integer_)
  tv_HH <- bdiag(e1@tv_HH, e2@tv_HH + xoffset, offdiag = NA_integer_)
  tv_Z <- bdiag(e1@tv_Z, e2@tv_Z + xoffset, offdiag = NA_integer_)
  tv_GG <- bdiag(e1@tv_GG, e2@tv_GG + xoffset, offdiag = NA_integer_)
  tv_HG <- bdiag(e1@tv_HG, e2@tv_HG + xoffset, offdiag = NA_integer_)
  ## cc requires special attention
  tv_cc <- rep(NA_integer_, length(e1@tv_cc))
  for (i in seq_along(e1@tv_cc)) {
    if (!is.na(e1@tv_cc[i]) & !is.na(e1@tv_cc[i])) {
      tv_cc[i] <- e1@tv_cc
      X[ , tv_cc[i]] <- e1@X[ , e1@tv_cc[i]] + e2@X[ , e2@tv_cc[i]]
      # this will leave an unused row in X. but too much of a pain to
      # delete it.
    } else if (!is.na(e1@tv_cc[i]) & is.na(e2@tv_cc[i])) {
      tv_cc[i] <- e1@tv_cc
      X[ , tv_cc[i]] <- X[ , e1@tv_cc[i]] + e2@cc[i]
    } else if (is.na(e1@tv_cc[i]) & !is.na(e2@tv_cc[i])) {
      tv_cc[i] <- e2@tv_cc + xoffset
      X[ , tv_cc[i]] <- e2@X[ , e2@tv_cc[i]] + e1@cc[i]
    }
  }
  tv_dd <- c(e1@tv_dd, e2@tv_dd)
  DLM(cc = cc, T = T, HH = HH,
      dd = dd, Z = Z, GG = GG, HG = HG,
      a1 = a1, P1 = P1,
      tv_cc = tv_cc, tv_T = tv_T, tv_HH = tv_HH,
      tv_dd = tv_dd, tv_Z = tv_Z, tv_GG = tv_GG,
      X = X)
}

setMethod("+", c(e1 = "DLM", e2 = "DLM"), add.DLM)
