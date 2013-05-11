#' @include yakf-package.R
#' @include DLM.R
#' @export dlm_polynomial
NULL

#' n-th order polynomial DLM
#'
#' The function creates an \code{\linkS4class{DLM}} object with
#' an n-th order polynomial.
#'
#' @param order Order of the polynomial model. \code{order = 1} is a local level model,
#' \code{order = 2} is a local trend model.
#' @return An object of class \code{\linkS4class{DLM}}.
#' @examples
#' # local level model
#' dlm_polynomial(matrix(1), 1, order = 1)
#' # local trend model
#' dlm_polynomial(diag(1, 2, 2), 1, order = 2)
#' # higer-order polynomial
#' dlm_polynomial(diag(1, 5, 5), 1, order = 5)
dlm_polynomial <- function(HH, GG, order = 1, a1 = NULL, P1 = NULL, cc = NULL,
                           dd = NULL, HG = NULL) {
  T <- matrix(0, order, order)
  for (i in 1:nrow(T)) {
    for (j in i:min(i + 1, ncol(T))) {
      T[i, j] <- 1
    }
  }
  Z <- diag(1, 1, order)
  DLM(T = T, HH = HH, Z = Z, GG = GG, a1 = a1, P1 = P1, cc = cc, dd = dd, HG = HG)
}
