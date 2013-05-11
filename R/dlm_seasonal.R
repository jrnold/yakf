dlm_seasonal <- function(s, sigma = 1, a1 = NULL, P1 = NULL) {
  Z <- diag(1, 1, s - 1)
  T <- rbind(rep(-1, s - 1), diag(1, s - 2, s - 1))
  GG <- matrix(0, 1, 1)
  HH <- matrix(c(sigma^2, rep(0, (s - 1)^2 - 1)),
               s - 1, s - 1)
  DLM(T = T, Z = Z, HH = HH, GG = GG)
}
