#' @export dlm_seasonal
dlm_seasonal <- function(s, sigma = 1, a1 = NULL, P1 = NULL) {
  Z <- Matrix(diag(1, 1, s - 1))
  T <- rBind(Matrix(-1, 1, s - 1), bandSparse(s - 2, s - 1, 0))
  GG <- Matrix(0, 1, 1)
  HH <- matrix(c(sigma^2, rep(0, (s - 1)^2 - 1)),
               s - 1, s - 1)
  DLM(T = T, Z = Z, HH = HH, GG = GG)
}
