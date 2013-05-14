#' @include DLM-class.R
#' @exportClass DLM
#' @export DLM
NULL

check_indices <- function(x, d, ncol_X, name) {
  if (ncol(x) != 3) {
    return(sprintf("ncol(%s) != 3"))
  }
  if (nrow(x) > 0) {
    if (mode(x) != "numeric") {
      return(sprintf("%s must be numeric", name))
    }
    if (! all(as.integer(x[ , 1]) %in% seq_len(d[1]))) {
      return(sprintf("invalid row indices for %s", name))
    }
    if (! all(as.integer(x[ , 2]) %in% seq_len(d[2]))) {
      return(sprintf("invalid column indices for %s", name))
    }
    if (! all(as.integer(x[ , 3]) %in% seq_len(ncol_X))) {
      return(sprintf("invalid X column index for %s", name))
    }
  }
  TRUE
}

dimlen <- function(x) {
  if (is.null(dim(x))) {
    length(x)
  } else {
    dim(x)
  }
}

#' @docType class
#' @rdname DLM-class
#' @aliases DLM-class
#' @aliases +,DLM,DLM-method
#' @title DLM class
#'
#' @description The \code{"DLM"} class represents a dynamic linear model,
#' also called state-space model.
#'
#' @param T \code{Matrix} of dimension m x m.
#' @param Z \code{Matrix} of dimension N x m.
#' @param HH \code{Matrix} of dimension m x m. 
#' @param GG \code{Matrix} of dimension N x N. 
#' @param P1 \code{Matrix} of dimension m x m. If \code{NULL}, then
#' a diffuse diagonal Matrix is used. The diagonal of this Matrix is set to 10^6 times the
#' maximum of 1 or values along the diagonal of \code{HH} and \code{GG}.
#' @param a1 \code{Matrix} of length m. If \code{NULL}, then set to a 0 vector.
#' @param dd \code{Matrix} of dimension N. If \code{NULL}, then set to a 0 vector.
#' @param cc \code{Matrix} of dimension m. If \code{NULL}, then set to a 0 vector.
#' @param HG \code{Matrix} of dimension m x N. If \code{NULL}, then it set to a 0 matrix.
#' @param tv_T \code{matrix} Indices of time-varying elements in \code{T}. See details.
#' @param tv_Z \code{matrix} Indices of time-varying elements in \code{Z}. See details.
#' @param tv_HH \code{matrix} Indices of time-varying elements in \code{HH}. See details.
#' @param tv_GG \code{matrix} Indices of time-varying elements in \code{GG}. See details. 
#' @param tv_cc \code{matrix} Indices of time-varying elements in \code{cc}. See details. 
#' @param tv_dd \code{matrix} Indices of time-varying elements in \code{dd}. See details. 
#' @param tv_HG \code{matrix} Indices of time-varying elements in \code{HG}. See details.
#' @param X \code{matrix} containing data used in time-varying parameters.
#' @return An object of class \code{\linkS4class{DLM}}.
#' @seealso \code{\linkS4class{DLM}}
#'
#' @section Slots:
#'
#' \describe{
#' \item{\code{T}}{\code{"Matrix"}. The \eqn{T} matrix.}
#' \item{\code{Z}}{\code{"Matrix"}. The \eqn{Z} matrix.}
#' \item{\code{HH}}{\code{"Matrix"}. The \eqn{H H'} matrix.}
#' \item{\code{GG}}{\code{"Matrix"}. The \eqn{G G'} matrix.}
#' \item{\code{P1}}{\code{"Matrix"}. The \eqn{P_1} matrix with the covariance of the initial states.}
#' \item{\code{a1}}{\code{"Matrix"}. The \eqn{a_1} vector with the mean of initial states.}
#' \item{\code{dd}}{\code{"Matrix"}. The \eqn{d} vector.}
#' \item{\code{cc}}{\code{"Matrix"}. The \eqn{c} vector.}
#' \item{\code{HG}}{\code{"Matrix"}. The \eqn{H G'} matrix.}
#' \item{\code{tv_T}}{\code{"matrix"}. Time varying indices for the \code{T} slot. See details.}
#' \item{\code{tv_Z}}{\code{"matrix"}. Time varying indices for the \code{Z} slot. See details.}
#' \item{\code{tv_HH}}{\code{"matrix"}. Time varying indices for the \code{HH} slot. See details.}
#' \item{\code{tv_GG}}{\code{"matrix"}. Time varying indices for the \code{GG} slot. See details.}
#' \item{\code{tv_cc}}{\code{"matrix"}. Time varying indices for the \code{cc} slot. See details.}
#' \item{\code{tv_dd}}{\code{"matrix"}. Time varying indices for the \code{dd} slot. See details.}
#' \item{\code{tv_HG}}{\code{"matrix"}. Time varying indices for the \code{HG} slot. See details.}
#' \item{\code{X}}{\code{"Matrix"}. Contains the time-varying parameters data.}
#' }
#'
#' @section Details:
#'
#' The structure and notation of DLM models in this class is based on that used in SsfPack
#' and \pkg{dlm}. See the package vignette for details.
#'
#'
#' @references
#' 
#' Giovanni Petris (2010), An R Package for Dynamic Linear Models.
#  Journal of Statistical Software, 36(12), 1-16.  \url{http://www.jstatsoft.org/v36/i12/}
#'
#' Petris, Petrone, and Campagnoli, Dynamic Linear Models with R, Springer (2009).
#'     
#' West and Harrison, Bayesian forecasting and dynamic models (2nd ed.), Springer (1997).
#'
setClass("DLM",
         representation(T = "Matrix",
                        Z = "Matrix",
                        HH = "Matrix",
                        GG = "Matrix",
                        P1 = "Matrix",
                        a1 = "Matrix",
                        dd = "Matrix",
                        cc = "Matrix",
                        HG = "Matrix",
                        tv_GG = "matrix",
                        tv_HH = "matrix",
                        tv_HG = "matrix",                        
                        tv_T = "matrix",
                        tv_Z = "matrix",
                        tv_cc = "matrix",
                        tv_dd = "matrix",
                        X = "Matrix"))

validity.DLM <- function(object) {
  m <- nrow(object@T) # number of states
  N <- nrow(object@Z) # number of columns

  matdims <- list(T = c(m, m),
                  Z = c(N, m),
                  HH = c(m, m),
                  GG = c(N, N),
                  a1 = c(m, 1L),
                  P1 = c(m, m),
                  dd = c(m, 1L),
                  cc = c(N, 1L),
                  HG = c(m, N))
  for (i in seq_along(matdims)) {
    slotname <- names(matdims)[i]
    if (! identical(matdims[[i]], dim(slot(object, slotname)))) {
      return(sprintf("Dimensions of slot %s != c(%d, %d)",
                     sQuote(slotname), matdims[[i]][1], matdims[[i]][2]))
    }
  }
  # Check for symmetric matrices
  for (name in c("HH", "GG", "P1")) {
    if (! isSymmetric(slot(object, name))) {
      return(sprintf("Matrix in slot %s is not symmetric", name))
    }
  }
  # Time varying slots must have same dimensions as parent slots
  for (i in c("GG", "HH", "T", "Z", "cc", "dd")) {
    tvslot <- sprintf("tv_%s", i)
    ret <- check_indices(slot(object, tvslot),
                         dimlen(slot(object, i)),
                         ncol(object@X), tvslot)
    if (is.character(ret)) {
      return(ret)
    }
  }
  TRUE
}

setValidity("DLM", validity.DLM)



empty_tv_idx <- function() matrix(nrow=0, ncol= 3)

#' @rdname DLM-class
DLM <- function(T, Z, HH, GG, a1 = NULL, P1 = NULL,
                cc = NULL, dd = NULL, HG = NULL,
                tv_T = NULL, tv_HH = NULL,
                tv_Z = NULL, tv_GG = NULL,
                tv_cc = NULL, tv_dd = NULL,
                tv_HG = NULL, X = NULL) {
  T <- Matrix(T)
  Z <- Matrix(Z)
  HH <- Matrix(HH)
  GG <- Matrix(GG)
  
  N <- nrow(Z)
  m <- nrow(T)
  # optional system matrices
  if (is.null(a1)) {
    a1 <- Matrix(0, m, 1)
  } else {
    a1 <- Matrix(a1)
  }
  kappa <- max(1, diag(tcrossprod(GG)), diag(tcrossprod(HH)))
  if (is.null(P1)) {
    P1 <- Diagonal(m, x = kappa * 10^6)
  } else {
    P1 <- Matrix(P1)
  }
  if (is.null(cc)) {
    cc <- Matrix(0, N, 1)
  } else {
    cc <- Matrix(cc)
  }
  if (is.null(dd)) {
    dd <- Matrix(0, m, 1)
  } else {
    dd <- Matrix(dd)
  }
  if (is.null(HG)) {
    HG <- Matrix(0, m, N)
  } else {
    HG <- Matrix(HG)
  }
  # time varying matrices
  if (is.null(tv_T)) tv_T <- empty_tv_idx()
  if (is.null(tv_Z)) tv_Z <- empty_tv_idx()
  if (is.null(tv_HH)) tv_HH <- empty_tv_idx()
  if (is.null(tv_GG)) tv_GG <- empty_tv_idx()
  if (is.null(tv_cc)) tv_cc <- empty_tv_idx()
  if (is.null(tv_dd)) tv_dd <- empty_tv_idx()
  if (is.null(tv_HG)) tv_HG <- empty_tv_idx()
  if (is.null(X)) {
    X <- Matrix(nrow=0, ncol=0)
  } else {
    X <- Matrix(X)
  }
  new("DLM", T = T, HH = HH, Z = Z, GG =GG,
      a1 = a1, P1 = P1, cc = cc, dd = dd, HG = HG,
      tv_T = tv_T, tv_HH = tv_HH,
      tv_Z = tv_Z, tv_GG = tv_GG, tv_HG = tv_HG,
      tv_cc = tv_cc, tv_dd = tv_dd,
      X = X)
}
