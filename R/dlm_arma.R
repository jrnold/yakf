#" @include yakf-package.R
#' @include DLM.R
#' @export dlm_arma
NULL

lengthen <- function(x, len = length(x), fill=0) {
  if (length(x) < len) {
    x <- c(x, rep(0, len - length(x)))
  }
  x
}

#' Create DLM object for an ARMA model
#'
#' @param ar AR Coefficients 
#' @param ma MA coefficients
#' @param sigma Standard deviation of MA error.
#' @return An object of class \code{"\linkS4class{DLM}"}.
#' @family create-DLM
#' @examples
#' dlm_arma(c(0.6, 0.2), -0.2, sqrt(0.9))
dlm_arma <- function(ar = 1, ma = 0, sigma = 1, a1 = NULL, P1 = NULL) {
  p <- length(ar)
  q <- length(ma)
  m <- max(p, q + 1)
  phi <- lengthen(ar, m)
  theta <- c(1, lengthen(ma, m - 1))
  # System matrices
  Z <- diag(1, 1, m)
  GG <- matrix(0, 1, 1)
  T <- cbind(phi, diag(1, m, m - 1))
  HH <- tcrossprod(theta) * sigma^2
  ## TODO: solve for initial conditions
  if (is.null(a1) && is.null(P1)) {
    a1 <- rep(0, m)
    TT <- kronecker(T, T)
    V <- solve(diag(1, nrow(TT), ncol(TT)) - TT, as.numeric(HH))
    P1 <- matrix(V, m, m)
  }
  DLM(T = T, Z = Z, HH = HH , GG = GG,
      cc = NULL, dd = NULL, HG = NULL,
      a1 = a1, P1 = P1)
}
