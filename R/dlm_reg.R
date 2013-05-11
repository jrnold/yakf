#' @include DLM.R
#' @export dlm_reg
NULL

eye <- function(m, n = m) {
  diag(1, m, n)
}

#' @title Create DLM object for a TVP regression model
#'
#' @description This function creates a \code{"DLM"} object
#' with a representation of a lienar regression model with
#' time-varying parameters.
#'
#' @param X \code{matrix} The design matrix.
#' @param GG \code{numeric} The observation variance.
#' @param HH \code{numeric} The system variance matrix. This should have
#' be a square matrix of dimensions \code{ncol(X) + 1} if \code{intercept = TRUE},
#' otherwise of dimension \code{ncol(X)}.
#' @param intercept \code{logical} Include an intercept?
#' @return An object of class \code{DLM}
#' @family create-dlm
#' @examples
#' x <- matrix(rnorm(6), ncol = 2)
#' dlm_reg(x)
dlm_reg <- function(X, GG = 1,
                    HH = if(intercept) diag(1, ncol(X) + 1, ncol(X) + 1)
                    else diag(1, ncol(X), ncol(X)),
                    intercept = TRUE, cc = NULL, dd = NULL,
                    a1 = NULL, P1 = NULL) {
  if (intercept) {
    Z <- matrix(1, 1, ncol(X) + 1)
    tv_Z <- matrix(c(NA_integer_, seq_len(ncol(X))), 1)
  } else {
    Z <- matrix(1, 1, ncol(X))
    tv_Z <- matrix(seq_len(ncol(X)), 1, ncol(X))
  }
  T <- eye(ncol(Z))
  DLM(T = T, Z = Z, HH = HH, GG = GG, cc = cc, dd = dd,
      a1 = a1, P1 = P1, X = X, tv_Z = tv_Z)
}
