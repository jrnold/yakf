#' @include yakf-package.R
#' @exportClass DLM
#' @export dlm_T dlm_Z dlm_HH dlm_GG dlm_HG dlm_cc dlm_dd
#' @export dlm_Sigma dlm_Omega dlm_delta dlm_Phi
NULL

#' @docType class
#' @rdname DLM-class
#' @aliases DLM-class
#' @title DLM class
#'
#' The \code{"DLM"} class represents a dynamic linear model,
#' also called state-space model.
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

#' rdname dlm-accessors
#' @aliases dlm_cc dlm_dd dlm_T dlm_Z dlm_HH dlm_HG dlm_Omega dlm_Phi dlm_Sigma dlm_delta
#' @title Access data from DLM objects
#'
#' These are some utility functions to access system matrices
#' from a DLM object. They are particularly useful if the object has
#' time varying parameters.
#'
#' @param object An object of class \code{DLM}.
#' @param i time
#' @return A \code{matrix} or \code{numeric} vector.
#'
#' @section Details:
#' @
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

dlm_cc <- function(object, i = NULL) get_tv_slot(object, "cc", i)

dlm_dd <- function(object, i = NULL) get_tv_slot(object, "dd", i)

dlm_T <- function(object, i = NULL) get_tv_slot(object, "T", i)

dlm_Z <- function(object, i = NULL) get_tv_slot(object, "Z", i)

dlm_HH <- function(object, i = NULL) get_tv_slot(object, "HH", i)

dlm_GG <- function(object, i = NULL) get_tv_slot(object, "GG", i)

dlm_HG <- function(object, i = NULL) get_tv_slot(object, "HG", i)

dlm_Omega <- function(object, i = NULL) {
  HH <- dlm_HH(object, i)
  GG <- dlm_GG(object, i)
  HG <- dlm_HG(object, i)
  rbind(cbind(HH, HG), cbind(t(HG), GG))
}

dlm_Phi <- function(object,i = NULL) {
  rbind(dlm_T(object, i), dlm_Z(object, i))
}

dlm_Sigma <- function(object) {
  rbind(object@P1, object@a1)
}

dlm_delta <- function(object, i = NULL) {
  matrix(c(dlm_dd(object, i), dlm_cc(object, i)))
}
