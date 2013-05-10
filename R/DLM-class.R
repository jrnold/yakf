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
                        HG = "matrix",
                        tv_GG = "matrix",
                        tv_HH = "matrix",
                        tv_T = "matrix",
                        tv_Z = "matrix",
                        tv_cc = "integer"
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
    tvslot <- sprintf("tv_%", i)
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
    if (any(is.na(slot(object, i)))) {
      if (nrow(object@X) < 1) {
        return(sprintf("object@%s has time-varying parameters, but object@X has no rows",
                       i))
      }
    }
  }
  TRUE
}

setValidity("DLM", validity.DLM)

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
    ind <- which(Negate(is.na)(tvv), arr.ind = TRUE)
    x[ind] <- object@X[i, as.integer(x[!is.na(x)])]
  }
  x
}

dlm_cc <- function(object, tv = NULL) get_tv_slot(object, "cc", tv)

dlm_dd <- function(object, tv = NULL) get_tv_slot(object, "dd", tv)

dlm_T <- function(object, tv = NULL) get_tv_slot(object, "T", tv)

dlm_Z <- function(object, tv = NULL) get_tv_slot(object, "Z", tv)

dlm_HH <- function(object, tv = NULL) get_tv_slot(object, "HH", tv)

dlm_GG <- function(object, tv = NULL) get_tv_slot(object, "GG", tv)

dlm_HG <- function(object, tv = NULL) get_tv_slot(object, "HG", tv)

dlm_Omega <- function(object, tv=NULL) {
  HH <- dlm(HH, tv)
  GG <- dlm(GG, tv)
  HG <- dlm(HG, tv)
  rbind(cbind(HH, HG), cbind(t(HG), GG))
}

dlm_phi <- function(object,tv = NULL) {
  rbind(dlm_T(object, tv), dlm_Z(object, tv))
}

dlm_Sigma <- function(object) {
  rbind(object@P1, object@a1)
}

dlm_delta <- function(object, tv = NULL) {
  matrix(c(dlm_dd(object, tv), dlm_cc(object, tv)))
}
