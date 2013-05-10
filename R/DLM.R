#' @include yakf-package.R
#' @include DLM-class.R
#' @export DLM
NULL

#' Create DLM objects
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
DLM <- function(T, HH, Z, GG, a1 = NULL, P1 = NULL,
                cc = NULL, dd = NULL, HG = NULL) {
  T <- as.matrix(T)
  HH <- as.matrix(HH)
  if (! is.matrix(Z)) {
    Z <- matrix(Z, 1, length(Z))
  }
  GG <- as.matrix(GG)
  N <- nrow(Z)
  m <- nrow(T)
  if (is.null(cc)) cc <- rep(0, N)
  if (is.null(dd)) dd <- rep(0, m)
  if (is.null(a1)) a1 <- rep(0, m)
  kappa <- max(1, diag(tcrossprod(GG)), diag(tcrossprod(HH)))
  if (is.null(P1)) P1 <- diag(kappa * 10^6, m, m)
  if (is.null(HG)) HG <- matrix(0, m, N)
  new("DLM", T = T, HH = HH, Z = Z, GG =GG,
      a1 = a1, P1 = P1, cc = cc, dd = dd, HG = HG)
}

