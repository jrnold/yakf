setClass("DLM",
         representation(T = "matrix",
                        Z = "matrix",
                        G = "matrix",
                        H = "matrix",
                        a1 = "numeric",
                        P1 = "matrix",
                        cc = "numeric",
                        dd = "numeric",
                        HG = "matrix"))

validity.DLM <- function(object) {
  m <- length(object@a1) # number of states
  N <- nrow(object@Z) # number of columns
  r <- ncol(object@H)
  if (length(object@cc) != N) {
    return(sprintf("length(object@cc) != %d", N))
  }
  if (length(object@dd) != m) {
    return(sprintf("length(object@dd) != %d", m))
  }
  if (dim(object@T) != c(m, m)) {
    return(sprintf("dim(object@T) != c(%d, %d)", m, m))
  }
  if (dim(object@Z) != c(N, m)) {
    return(sprintf("dim(object@Z) != c(%d, %d)", N, m))
  }
  if (length(object@a1) != m) {
    return(sprintf("length(object@a1) != c(%d)", m))
  }
  if (dim(object@P1) != c(m, m)) {
    return(sprintf("dim(object@Z) != c(%d, %d)", m, m))
  }
  if (dim(object@G) != c(N, r)) {
    return(sprintf("dim(object@G) != c(%d, %d)", N, r))
  }
  if (dim(object@H) != c(m, r)) {
    return(sprintf("dim(object@H) != c(%d, %d)", m, r))
  }
  if (dim(object@HG) != c(m, N)) {
    return(sprintf("dim(object@H) != c(%d, %d)", m, N))
  }
}

DLM <- function(T, H, Z, G, a1 = NULL, P1 = NULL, cc = NULL,
                dd = NULL, HG = NULL) {
  T <- as.matrix(T)
  H <- as.matrix(H)
  Z <- as.matrix(Z)
  G <- as.matrix(G)
  N <- nrow(Z)
  m <- nrow(T)
  if (is.null(cc)) cc <- rep(0, N)
  if (is.null(dd)) dd <- rep(0, m)
  if (is.null(a1)) a1 <- rep(0, m)
  kappa <- max(1, diag(tcrossprod(G)), diag(tcrossprod(H)))
  if (is.null(P1)) P1 <- diag(kappa * 10^6, m, m)
  if (is.null(HG)) HG <- matrix(0, m, N)
  new("DLM", T = T, H = H, Z = Z, G =G,
      a1 = a1, P1 = P1, cc = cc, dd = dd, HG = HG)
}

