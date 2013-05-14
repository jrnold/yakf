#' @include DLM-class.R
#' @export dlm_polynomial
NULL

#' n-th order polynomial DLM
#'
#' The function creates an \code{\linkS4class{DLM}} object with
#' an n-th order polynomial.
#'
#' @param order \code{integer} Order of the polynomial model. \code{order = 1} is a local level model,
#' \code{order = 2} is a local trend model.
#' @param GG \code{numeric} Observation variance, \eqn{G G' = \sigma^2}.
#' @return An object of class \code{\linkS4class{DLM}}.
#' @family create-DLM
#' @examples
#' # local level model
#' dlm_polynomial(1, GG = 2)
#' # local trend model
#' dlm_polynomial(2, GG = 2)
#' # higer-order polynomial
#' dlm_polynomial(order = 5, GG = 3)
dlm_polynomial <- function(order = 1, GG = 1,
                           HH = NULL,
                           a1 = NULL, P1 = NULL,
                           cc = NULL, dd = NULL,
                           HG = NULL) {
  if (order == 1) {
    T <- Matrix(1, 1, 1)
  } else {
    T <- bandSparse(order, order, k = c(0, 1),
                    list(rep(1, order), rep(1, order - 1)))
  }
  Z <- Matrix(diag(1, 1, order))
  ## Default variance is integrated polynomial with variance 1
  HH <- Matrix(0, order, order)
  HH[order, order] <- 1
  DLM(T = T, HH = HH, Z = Z, GG = GG, a1 = a1, P1 = P1, cc = cc, dd = dd, HG = HG)
}
