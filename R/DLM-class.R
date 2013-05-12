#' @include yakf-package.R
#' @exportClass DLM
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
#' @title DLM class
#'
#' @description The \code{"DLM"} class represents a dynamic linear model,
#' also called state-space model.
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
