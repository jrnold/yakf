#' @include yakf-package.R
#' @include DLM-class.R
#' @export DLM
NULL

empty_tv_idx <- function() matrix(nrow=0, ncol= 3)

#' Create DLM objects
#'
#' This is a utility function to create objects of class
#' \code{\linkS4class{DLM}}.
#'
#' @param T \code{matrix} of dimension m x m.
#' @param Z \code{matrix} of dimension N x m.
#' @param HH \code{matrix} of dimension m x m. 
#' @param GG \code{matrix} of dimension N x N. 
#' @param P1 \code{matrix} of dimension m x m. If \code{NULL}, then
#' a diffuse diagonal matrix is used. The diagonal of this matrix is set to 10^6 times the
#' maximum of 1 or values along the diagonal of \code{HH} and \code{GG}.
#' @param a1 \code{numeric} of length m. If \code{NULL}, then set to a 0 vector.
#' @param dd \code{numeric} of dimension N. If \code{NULL}, then set to a 0 vector.
#' @param cc \code{numeric} of dimension m. If \code{NULL}, then set to a 0 vector.
#' @param HG \code{matrix} of dimension m x N. If \code{NULL}, then set to a 0 matrix.
#' @param tv_T \code{matrix} Indices of time-varying elements in \code{T}. See details.
#' @param tv_Z \code{matrix} Indices of time-varying elements in \code{Z}. See details.
#' @param tv_HH \code{matrix} Indices of time-varying elements in \code{HH}. See details.
#' @param tv_GG \code{matrix} Indices of time-varying elements in \code{GG}. See details. 
#' @param tv_cc \code{matrix} Indices of time-varying elements in \code{cc}. See details. 
#' @param tv_dd \code{matrix} Indices of time-varying elements in \code{dd}. See details. 
#' @param tv_HG \code{matrix} Indices of time-varying elements in \code{HG}. See details.
#' @param X \code{matrix} containing data used in time-varying parameters.
#' @return An object of class \code{\linkS4class{DLM}}.
#' @seealso \code{\linkS4class{DLM}}
#' @family create-dlm
DLM <- function(T, Z, HH, GG, a1 = NULL, P1 = NULL,
                cc = NULL, dd = NULL, HG = NULL,
                tv_T = NULL, tv_HH = NULL,
                tv_Z = NULL, tv_GG = NULL,
                tv_cc = NULL, tv_dd = NULL,
                tv_HG = NULL, X = NULL) {
  T <- Matrix(T)
  Z <- Matrix(Z)
  HH <- Matrix(HH)
  GG <- Matrix(GG)
  
  N <- nrow(Z)
  m <- nrow(T)
  # optional system matrices
  if (is.null(a1)) {
    a1 <- Matrix(0, m, 1)
  } else {
    a1 <- Matrix(a1)
  }
  kappa <- max(1, diag(tcrossprod(GG)), diag(tcrossprod(HH)))
  if (is.null(P1)) {
    P1 <- Diagonal(m, x = kappa * 10^6)
  } else {
    P1 <- Matrix(P1)
  }
  if (is.null(cc)) {
    cc <- Matrix(0, N, 1)
  } else {
    cc <- Matrix(cc)
  }
  if (is.null(dd)) {
    dd <- Matrix(0, m, 1)
  } else {
    dd <- as.numeric(dd)
  }
  if (is.null(HG)) {
    HG <- Matrix(0, m, N)
  } else {
    HG <- Matrix(HG)
  }
  # time varying matrices
  if (is.null(tv_T)) tv_T <- empty_tv_idx()
  if (is.null(tv_Z)) tv_Z <- empty_tv_idx()
  if (is.null(tv_HH)) tv_HH <- empty_tv_idx()
  if (is.null(tv_GG)) tv_GG <- empty_tv_idx()
  if (is.null(tv_cc)) tv_cc <- empty_tv_idx()
  if (is.null(tv_dd)) tv_dd <- empty_tv_idx()
  if (is.null(tv_HG)) tv_HG <- empty_tv_idx()
  if (is.null(X)) {
    X <- Matrix(nrow=0, ncol=0)
  } else {
    X <- Matrix(X)
  }
  new("DLM", T = T, HH = HH, Z = Z, GG =GG,
      a1 = a1, P1 = P1, cc = cc, dd = dd, HG = HG,
      tv_T = tv_T, tv_HH = tv_HH,
      tv_Z = tv_Z, tv_GG = tv_GG, tv_HG = tv_HG,
      tv_cc = tv_cc, tv_dd = tv_dd,
      X = X)
}
