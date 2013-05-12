setClass("KalmanFilter",
         representation(v = "Matrix",
                        K = "list",
                        Finv = "list"))

validity.KalmanFilter <- function(object) {
  N <- nrow(v)
  n <- ncol(V)
  if (length(object@K) != n) {
    return("ncol(K) != ncol(v)")
  }
  if (length(object@Finv) != n) {
    return("length(Finv) != ncol(v)")
  }
  # Check K
  if (!all(sapply(object@K, is, class2 = "Matrix"))) {
    return("class(x) != 'Matrix' for some element in K")
  }
  if (!all(sapply(object@K, function(x) ncol(x) == N))) {
    return("nrow(x) != nrow(v) for some element in K")
  }
  m <- nrow(object@K[[1]])
  if (!all(sapply(object@K, function(x) nrow(x) == m))) {
    return("Elements in K do not have equal rows")
  }

  # Check Finv
  if (!all(sapply(object@Finv, nrow, class2 = "Matrix"))) {
    return("class(x) != 'Matrix' for some element in Finv")
  }
  if (!all(sapply(object@Finv, function(x) nrow(x) == N))) {
    return("nrow(x) != 'N' for some element in Finv")
  }
  if (!all(sapply(object@Finv, function(x) ncol(x) == N))) {
    return("ncol(x) != 'N' for some element in Finv")
  }
  TRUE
}

check

         
