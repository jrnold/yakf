#' @include DLM-class.R
#' @export dlm_arima
NULL

lengthen <- function(x, len = length(x), fill=0) {
  if (length(x) < len) {
    x <- c(x, rep(0, len - length(x)))
  }
  x
}

#' Create DLM object for an ARMA model
#'
#' \code{dlm_arima} creates state space representations of ARIMA models.
#'
#' @param ar AR Coefficients 
#' @param ma MA coefficients
#' @param sigma Standard deviation of MA error.
#' @param ... Arguments passed to \code{dlm_arima}.
#' @return An object of class \code{"\linkS4class{DLM}"}.
#' 
#' @section Details:
#'
#' If both \code{a1} and \code{P1} are \code{NULL} then \code{a1} = 0,
#' and \code{P1} is set to the steady state value of \code{P}.
#' 
#' @family create-DLM
#' @examples
#' # an ARMA(2, 1)
#' dlm_arima(c(0.6, 0.2), -0.2, sqrt(0.9))
#' # an ARIMA(2, 1, 1) model
#' dlm_arima(c(0.6, 0.2), -0.2, sqrt(0.9), d = 1)
dlm_arima <- function(ar = 1, ma = 0, d = 0, sigma = 1, GG = 0, a1 = NULL, P1 = NULL,
                     cc = NULL, dd = NULL) {
  p <- length(ar)
  q <- length(ma)
  m <- max(p, q + 1)
  phi <- Matrix(lengthen(ar, m), m, 1)
  theta <- Matrix(c(1, lengthen(ma, m - 1)), m, 1)
  # System matrices
  Z <- Matrix(c(rep(1, d + 1), rep(0, m - 1)), nrow = 1)
  T1 <- cBind(triu(Matrix(1, d, d)), Matrix(1, d, 1), Matrix(0, d, m - 1))
  T2 <- cBind(phi, bandSparse(m, m - 1, 0, list(rep(1, m))))
  T <- rBind(T1, cBind(Matrix(0, m, d), T2))
  H2 <- tcrossprod(theta) * sigma^2
  HH <- bdiag(Matrix(0, d, d), H2)
  ## TODO: solve for initial conditions
  if (is.null(a1) && is.null(P1)) {
    a1 <- Matrix(0, m + d, 1)
    TT <- Matrix::kronecker(T2, T2)
    V <- solve(Matrix(diag(1, nrow(TT), ncol(TT))) - TT, as.numeric(H2))
    P1 <- bdiag(Diagonal(d, 10e6), Matrix(as.numeric(V), m, m))
  }
  DLM(T = T, Z = Z, HH = HH , GG = GG, cc = NULL, dd = NULL, HG = NULL,
      a1 = a1, P1 = P1)
}
