#' @include SSM-class.R
#' @export ssm_polynomial
NULL

#' n-th order polynomial SSM
#'
#' The function creates an \code{\linkS4class{SSM}} object with
#' an n-th order polynomial.
#'
#' @param order \code{integer} Order of the polynomial model. \code{order = 1} is a local level model,
#' \code{order = 2} is a local trend model.
#' @param H \code{numeric} Observation variance.
#' @param Q \code{numeric} System variance.
#' @param ... Arguments passed to \code{\link{SSM}}.
#' @return An object of class \code{\linkS4class{SSM}}.
#' @family create-SSM
#' @examples
#' # local level model
#' ssm_poly(1, GG = 2)
#' # local trend model
#' ssm_poly(2, GG = 2)
#' # higer-order polynomial
#' ssm_poly(order = 5, GG = 3)
ssm_poly <- function(order = 1L, H = 1, Q = NULL, ...) {
  order <- as.integer(order)
  if (order == 1) {
    T <- Matrix(1, 1, 1)
  } else {
    T <- bandSparse(order, order, k = c(0, 1),
                    list(rep(1, order), rep(1, order - 1)))
  } 
  Z <- Matrix(diag(1, 1, order))
  ## Default variance is integrated polynomial with variance 1
  if (is.null(Q)) {
    Q <- Matrix(0, order, order)
    Q[order, order] <- 1
  }
  SSM(T = T, H = H, Z = Z, Q = Q, ...)
}
