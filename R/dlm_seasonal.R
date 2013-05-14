#' @include DLM-class.R
#' @export dlm_seasonal
NULL

#' Create DLM for seasonal model
#'
#' @param s Number of seasons
#' @param sigma Standard deviation for the seasonal state.
#' @return An object of class \code{\linkS4class{DLM}}.
#' @family create-DLM
#' @examples
#' # DLM with 4 seasons
#' dlm_seasonal(4)
dlm_seasonal <- function(s, sigma = 1, GG = 1, a1 = NULL, P1 = NULL,
                         cc = NULL, dd = NULL) {
  Z <- Matrix(diag(1, 1, s - 1))
  T <- rBind(Matrix(-1, 1, s - 1), bandSparse(s - 2, s - 1, 0, list(rep(1, s - 2))))
  HH <- matrix(c(sigma^2, rep(0, (s - 1)^2 - 1)), s - 1, s - 1)
  DLM(T = T, Z = Z, HH = HH, GG = GG)
}
