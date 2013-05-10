#' @include yakf-package.R
#' @exportClass DLM
NULL

setClass("DLM",
         representation(T = "matrix",
                        Z = "matrix",
                        GG = "matrix",
                        HH = "matrix",
                        a1 = "numeric",
                        P1 = "matrix",
                        cc = "numeric",
                        dd = "numeric",
                        HG = "matrix"))

validity.DLM <- function(object) {
  m <- nrow(object@T) # number of states
  N <- nrow(object@Z) # number of columns
  if (length(object@cc) != N) {
    return(sprintf("length(object@cc) != %d", N))
  }
  if (length(object@dd) != m) {
    return(sprintf("length(object@dd) != %d", m))
  }
  if (! identical(dim(object@T), c(m, m))) {
    return(sprintf("dim(object@T) != c(%d, %d)", m, m))
  }
  if (! identical(dim(object@Z), c(N, m))) {
    return(sprintf("dim(object@Z) != c(%d, %d)", N, m))
  }
  if (length(object@a1) != m) {
    return(sprintf("length(object@a1) != c(%d)", m))
  }
  if (! identical(dim(object@P1), c(m, m))) {
    return(sprintf("dim(object@Z) != c(%d, %d)", m, m))
  }
  if (! identical(dim(object@GG), c(N, N))) {
    return(sprintf("dim(object@GG) != c(%d, %d)", N, N))
  }
  if (! identical(dim(object@HH), c(m, m))) {
    return(sprintf("dim(object@HH) != c(%d, %d)", m, m))
  }
  if (! identical(dim(object@HG), c(m, N))) {
    return(sprintf("dim(object@HH) != c(%d, %d)", m, N))
  }
  TRUE
}

setValidity("DLM", validity.DLM)
dlm_Omega <- function(object) {
  rbind(cbind(object@HH, object@HG), cbind(t(object@HG), object@GG))
}

dlm_phi <- function(object) {
  rbind(object@T, object@Z)
}

dlm_Sigma <- function(object) {
  rbind(object@P1, object@a1)
}

dlm_delta <- function(object) {
  matrix(c(object@dd, object@cc))
}
