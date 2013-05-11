#' @include yakf-package.R
#' @exportClass DLM
NULL

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
#' \item{\code{T}}{\code{"matrix"}. The \eqn{T} matrix.}
#' \item{\code{Z}}{\code{"matrix"}. The \eqn{Z} matrix.}
#' \item{\code{GG}}{\code{"matrix"}. The \eqn{G G'} matrix.}
#' \item{\code{HH}}{\code{"matrix"}. The \eqn{H H'} matrix.}
#' \item{\code{a1}}{\code{"matrix"}. The \eqn{a_1} vector with the mean of initial states.}
#' \item{\code{P1}}{\code{"matrix"}. The \eqn{P_1} matrix with the covariance of the initial states.}
#' \item{\code{cc}}{\code{"numeric"}. The \eqn{c} vector.}
#' \item{\code{dd}}{\code{"numeric"}. The \eqn{d} vector.}
#' \item{\code{HG}}{\code{"matrix"}. The \eqn{H G'} matrix.}
#' \item{\code{tv_GG}}{\code{"matrix"}. Time varying indices for the \code{GG} slot. See details.}
#' \item{\code{tv_HH}}{\code{"matrix"}. Time varying indices for the \code{HH} slot. See details.}
#' \item{\code{tv_HG}}{\code{"matrix"}. Time varying indices for the \code{HG} slot. See details.}
#' \item{\code{tv_T}}{\code{"matrix"}. Time varying indices for the \code{T} slot. See details.}
#' \item{\code{tv_Z}}{\code{"matrix"}. Time varying indices for the \code{Z} slot. See details.}
#' \item{\code{tv_cc}}{\code{"integer"}. Time varying indices for the \code{cc} slot. See details.}
#' \item{\code{tv_dd}}{\code{"integer"}. Time varying indices for the \code{dd} slot. See details.}
#' \item{\code{X}}{\code{"matrix"}. Contains the time-varying parameters data.}
#' }
#'
#' @section Details:
#'
#' The internal structure of this class is based on that used in SsfPack
#' and \pkg{dlm}. See the package vignette for details.
#'
setClass("DLM",
         representation(T = "matrix",
                        Z = "matrix",
                        GG = "matrix",
                        HH = "matrix",
                        a1 = "numeric",
                        P1 = "matrix",
                        cc = "numeric",
                        dd = "numeric",
                        HG = "matrix",
                        tv_GG = "matrix",
                        tv_HH = "matrix",
                        tv_HG = "matrix",                        
                        tv_T = "matrix",
                        tv_Z = "matrix",
                        tv_cc = "integer",
                        tv_dd = "integer",
                        X = "matrix"))

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
  # check that matrices are numeric
  for (i in c("T", "Z", "GG", "HH", "P1", "HG",
              "tv_T", "tv_T", "tv_GG", "tv_HH")) {
    if (mode(slot(object, i)) != "numeric") {
      return(sprintf("object@%s does not have storage mode numeric", i))
    }
  }
  
  # Time varying slots must have same dimensions as parent slots
  for (i in c("GG", "HH", "T", "Z", "cc", "dd")) {
    tvslot <- sprintf("tv_%s", i)
    if (! identical(dim(slot(object, tvslot)), dim(slot(object, i)))) {
      return(sprintf("slots %s and %s do not have the same dimension",
                     i, tvslot))
    }
  }
  # Entries in time-varying parameters must point to a row in X
  for (i in paste0("tv_", c("GG", "HH", "T", "Z", "cc", "dd"))) {
    # Check that entries are valid
    for (j in unique(Filter(Negate(is.na), as.numeric(slot(object, i))))) {
      if ((as.integer(j) < 1) || (as.integer(j) > ncol(object@X))) {
        return(sprintf("value %d in object@%s is invalid",
                       as.integer(j), i))
      }
    }
    # If there are any time-varying parameters, object@X must have some rows
    if (any(! is.na(slot(object, i)))) {
      if (nrow(object@X) < 1) {
        return(sprintf("object@%s has time-varying parameters, but object@X has no rows",
                       i))
      }
    }
  }
  TRUE
}

setValidity("DLM", validity.DLM)



## Accessor Utility Functions
get_tv_slot <- function(object, slot, i) {
  if (length(i) > 1) {
    i <- i[1]
    warning("only first element of i used")
  }
  tvi <- slot(object, sprintf("tv_%s", i))
  if (is.null(i) || (all(is.na(tvi)))) {
    x <- slot(object, i)
  } else {
    x <- slot(object, i)
    ind <- which(Negate(is.na)(tvi), arr.ind = TRUE)
    x[ind] <- object@X[i, as.integer(x[!is.na(x)])]
  }
  x
}
