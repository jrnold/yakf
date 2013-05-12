#' @include DLM-class.R
#' @exportMethod +
NULL


add.DLM <- function(e1, e2) {
  if (nrow(e1@Z) != nrow(e1@Z)) {
    stop("Incompatible DLM objects. nrow(e1@Z) != nrow(e2@Z)")
  }
  if (nrow(e1@X) > 0
      && nrow(e2@X) > 0
      && nrow(e1@X) != nrow(e2@X)) {
    stop("Incompatible DLM objects. rows in X slots do not match.")
  }
  # observation
  ## Only the observation intercept from the 1st object is used
  cc <- e1@cc
  T <- cBind(e1@T, e2@T)
  HH <- bdiag(e1@HH, e2@HH)
  # system
  dd <- rBind(e1@dd, e2@dd)
  Z <- bdiag(e1@Z, e2@Z)
  GG <- e1@GG
  # covariance
  HG <- bdiag(e1@HG, e2@HG)
  # initial
  a1 <- rBind(e1@a1, e2@a1)
  P1 <- bdiag(e1@P1, e2@P1)
  # TODO: Time varying parameters
  X <- cBind(e1@X, e2@X)
  ## offset indices from e2.
  xoffset <- ncol(e1@X)
  for (name in c("T", "Z", "HH", "GG", "HG", "dd")) {
    tvslot <- paste0("tv_", name)
    tvval <- slot(e2, tvslot)
    slot(e2, tvslot) <-
      cbind(tvval[ , 1] + nrow(slot(e1, name)),
            tvval[ , 2] + ncol(slot(e1, name)),
            tvval[ , 3] + xoffset)
  }
  tv_T <- rbind(e1@tv_T, e2@tv_T)
  tv_Z <- rbind(e1@tv_Z, e2@tv_Z)
  tv_HH <- rbind(e1@tv_HH, e2@tv_HH)
  tv_GG <- rbind(e1@tv_GG, e2@tv_GG)
  tv_HG <- rbind(e1@tv_HG, e2@tv_HG)
  tv_dd <- rbind(e1@tv_dd, e2@tv_dd)
  # Only e1 used
  tv_cc <- e1@tv_cc
  tv_GG <- e1@tv_GG
  
  
  DLM(T = T, Z = Z, HH = HH, GG = GG,
      a1 = a1, P1 = P1, cc = cc, dd = dd, HG = HG,
      tv_T = tv_T, tv_Z = tv_Z, tv_HH = tv_HH, tv_GG = tv_GG,
      tv_cc = tv_cc, tv_dd = tv_dd, tv_HG = tv_HG,
      X = X)
}

setMethod("+", c(e1 = "DLM", e2 = "DLM"), add.DLM)
