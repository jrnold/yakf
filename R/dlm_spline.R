#' @include DLM.R
#' @export dlm_spline
NULL

#' Create DLM representation of Cubic Spline Smoother
#'
#' @param q Signal to noise ratio (smoothness penalty).
#' @param delta distance between points
#' @param a1 passed to \code{\link{DLM}}.
#' @param P1 passed to \code{\link{DLM}}.
#' @return An object of class \code{"\linkS4class{DLM}"}.
#' @family create-DLM
#' @examples
#' # cubic spline with equally spaced observations
#' # and signal-to-noise ratio of 0.2
#' dlm_spline(0.2)
#' # cubic spline with non-equally spaced observations
#' dlm_spline(0.2, diff(c(1, 2, 3, 5, 9, 12, 17, 20, 23, 25)))
dlm_spline <- function(q, delta = 1, a1 = NULL, P1 = NULL) {
  if (length(q) > 1) {
    q <- q[1]
    warning("only first element of 'q' used.")
  }
  if (q <= 0) {
    stop("'q' must be greater than 0.")
  }
  if (any(delta < 0)) {
    stop("'delta' must be >= 0")
  }
  Z <- matrix(c(1, 0), 1, 2)
  GG <- matrix(1, 1, 1)
  if (length(delta) == 1) {
    HH <- q * matrix(c(delta^3 / 3, delta^2 / 2,
                       delta^2 / 2, delta), 2, 2)
    T <- matrix(c(1, 0, delta, 1), 2, 2)
    X <- NULL
    tv_T <- NULL
    tv_HH <- NULL
  } else {
    X <- cbind(delta, q * delta, q * delta^2 / 2, q * delta^3 / 3)
    T <- matrix(c(1, 0, 0, 1), 2, 2)
    tv_T <- matrix(NA, 2, 2)
    tv_T[1, 2] <- 1L
    HH <- matrix(0, 2, 2)
    tv_HH <- matrix(c(4L, 3L, 3L, 2L), 2, 2)
  }
  DLM(T = T, Z = Z, HH = HH, GG = GG, tv_T = tv_T, tv_HH = tv_HH, X = X,
      a1 = NULL, P1 = NULL)
}
