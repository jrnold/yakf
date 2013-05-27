#' @include SSM-class.R
#' @export ssm_reg
NULL

#' @title Create SSM object for a TVP regression model
#'
#' @description This function creates a \code{"SSM"} object
#' with a representation of a lienar regression model with
#' time-varying parameters.
#'
#' @param X \code{Matrix} The design matrix.
#' @param H \code{Matrix} The observation variance.
#' @param Q \code{Matrix} The system variance matrix. This should have
#' be a square matrix of dimensions \code{ncol(X) + 1} if \code{intercept = TRUE},
#' otherwise of dimension \code{ncol(X)}.
#' @param rho \code{numeric} AR(1) coefficients for the regression parameters.
#' By default the regression parameters follow a random walk.
#' @param ... Arguments passed to \code{\link{SSM}}.
#' @param intercept \code{logical} Include an intercept?
#' @return An object of class \code{SSM}
#' @family create-SSM
#' @examples
#' x <- matrix(rnorm(6), ncol = 2)
#' ssm_reg(x)
ssm_reg <- function(X, intercept = TRUE, H = 1, Q = NULL, rho = 1, ...) {
  if (intercept) {
    X <- cBind(1, Matrix(X))
  }
  Z <- MatrixList(lapply(seq_len(nrow(X)), function(i) X[i, , drop = FALSE]))
  T <- Diagonal(ncol(Z), rho)
  if (is.null(Q)) {
    Q <- Diagonal(ncol(Z))
  }
  SSM(T = T, Z = Z, H = H, Q = Q, ...)
}
