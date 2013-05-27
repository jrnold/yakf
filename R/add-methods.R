#' @include bd-methods.R
#' @include cbind2-methods.R
#' @include rbind2-methods.R
#' @include Ops-methods.R
#' @exportMethod +
NULL

`+.SSM` <- function(e1, e2) {
  n1 <- dim(e1)['n']
  n2 <- dim(e2)['n']
  if ((n1 && n2) && (n1 != n2)) {
    stop("number of time periods in objects are not equal")
  }
  SSM(Z = cBind(e1@Z, e2@Z),
      T = e1@T %bd% e2@T,
      H = e1@H + e2@H,
      Q = e1@Q %bd% e2@Q,
      R = e1@R %bd% e2@R,
      a1 = rBind(e1@a1, e2@a1),
      P1 = bdiag(e1@P1, e2@P1),
      cc = e1@cc + e2@cc,
      dd = rBind(e1@dd, e2@dd))
}
