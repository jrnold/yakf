#' @include yakf-package.R
#' @include DLM-class.R
#' @export DLM
NULL

namask <- function(x) {
  if (is.matrix(x)) {
    matrix(NA_integer_, nrow(x), ncol(x))
  } else {
    rep(NA_integer_, length(x))
  }
}

#' Create DLM objects
#'
#' This is a utility function to create objects of class
#' \code{\linkS4class{DLM}}.
#'
#' @param T \code{matrix} of dimension m x m.
#' @param HH \code{matrix} of dimension m x m.
#' @param Z \code{matrix} of dimension N x m.
#' @param GG \code{matrix} of dimension N x N.
#' @param a1 \code{numeric} of length m.
#' @param P1 \code{matrix} of dimension m x m.
#' @param cc \code{numeric} of dimension m.
#' @param dd \code{numeric} of dimension N.
#' @param HG \code{matrix} of dimension m x N.
#' @param tv_T \code{matrix} of same dimension as \code{T}.
#' @param tv_HH \code{matrix} of same dimension as \code{HH}.
#' @param tv_Z \code{matrix} of same dimension as \code{Z}.
#' @param tv_GG \code{matrix} of same dimension as \code{GG}.
#' @param tv_cc \code{matrix} of same dimension as \code{cc}.
#' @param tv_dd \code{matrix} of same dimension as \code{dd}.
#' @param tv_HG \code{matrix} of same dimension as \code{HG}.
#' @param X \code{matrix} containing time varying data.
#' @return An object of class \code{\linkS4class{DLM}}.
#' @seeAlso \code{\linkS4class{DLM}} for a description of DLM objects.
DLM <- function(T, HH, Z, GG, a1 = NULL, P1 = NULL,
                cc = NULL, dd = NULL, HG = NULL,
                tv_T = NULL, tv_HH = NULL,
                tv_Z = NULL, tv_GG = NULL,
                tv_cc = NULL, tv_dd = NULL,
                tv_HG = NULL, X = NULL) {
  T <- as.matrix(T)
  HH <- as.matrix(HH)
  if (! is.matrix(Z)) {
    Z <- matrix(Z, 1, length(Z))
  }
  GG <- as.matrix(GG)
  
  N <- nrow(Z)
  m <- nrow(T)
  # optional system matrices
  if (is.null(cc)) cc <- rep(0, N)
  if (is.null(dd)) dd <- rep(0, m)
  if (is.null(a1)) a1 <- rep(0, m)
  kappa <- max(1, diag(tcrossprod(GG)), diag(tcrossprod(HH)))
  if (is.null(P1)) P1 <- diag(kappa * 10^6, m, m)
  if (is.null(HG)) HG <- matrix(0, m, N)
  # time varying matrices
  if (is.null(tv_T)) tv_T <- namask(TV)
  if (is.null(tv_HH)) tv_HH <- namask(HH)
  if (is.null(tv_Z)) tv_Z <- namask(Z)
  if (is.null(tv_GG)) tv_GG <- namask(GG)
  if (is.null(tv_cc)) tv_Z <- namask(cc)
  if (is.null(tv_dd)) tv_Z <- namask(dd)
  if (is.null(tv_HG)) tv_HG <- namask(HG)
  if (is.null(X)) <- matrix(nrow=0, ncol=0)
  new("DLM", T = T, HH = HH, Z = Z, GG =GG,
      a1 = a1, P1 = P1, cc = cc, dd = dd, HG = HG,
      tv_T = tv_T, tv_HH = tv_HH,
      tv_Z = tv_Z, tv_GG = tv_GG,
      tv_cc = tv_cc, tv_dd = tv_dd,
      X = X)
}

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
  a <- c(e1@a1, e2@a1)
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

setMethod("+", c(e1 = "DLM", e2 = "DLM"),
          add.dlm)

