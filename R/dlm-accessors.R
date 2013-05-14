#' @include DLM-class.R
#' @export dlm_T dlm_Z dlm_HH dlm_GG dlm_HG dlm_cc dlm_dd
#' @export dlm_Sigma dlm_Omega dlm_delta dlm_Phi
#' @export dlm_obs dlm_vars dlm_states
NULL

## Accessor Utility Functions
get_tv_slot <- function(object, slotname, i) {
  if (length(i) > 1) {
    i <- i[1]
    warning("only first element of i used")
  }
  tvi <- slot(object, sprintf("tv_%s", slotname))
  if (is.null(i) || (! nrow(tvi))) {
    x <- slot(object, slotname)
  } else {
    x <- slot(object, slotname)
    x[tvi[ , 1], tvi[ , 2]] <- object@X[i, tvi[ , 3]]
  }
  x
}

#' @name dlm-accessors
#' @rdname dlm-accessors
#' @aliases dlm_cc dlm_dd dlm_T dlm_Z dlm_HH dlm_GG
#' @aliases dlm_HG dlm_Omega dlm_Phi dlm_Sigma dlm_delta
#' @aliases dlm_states dlm_obs dlm_vars
#' @title Access data from DLM objects
#'
#' @description These are some utility functions to access system matrices
#' from a DLM object. They are particularly useful if the object has
#' time varying parameters.
#'
#' @param object An object of class \code{DLM}.
#' @param i time
#' @return A \code{matrix} or \code{numeric} vector.
#'
#' @section Details:
#' \describe{
#' \item{\code{dlm_cc}}{Return \eqn{c_t} vector}
#' \item{\code{dlm_dd}}{Return \eqn{d_t} vector}
#' \item{\code{dlm_T}}{Return \eqn{T_t} matrix}
#' \item{\code{dlm_Z}}{Return \eqn{Z_t} matrix}
#' \item{\code{dlm_HH}}{Return \eqn{H_t H_t'} matrix}
#' \item{\code{dlm_GG}}{Return \eqn{G_t G_t'} matrix}
#' \item{\code{dlm_HG}}{Return \eqn{H_t G_t'} matrix}
#' \item{\code{dlm_Omega}}{Return the matrix,
#' \deqn{\Omega_t = ( H_t H_t' ,  H_t G_t' ; G_t H_t' , G_t G_t' )}
#' }
#' \item{\code{dlm_delta}}{Return the matrix \deqn{\delta_t = ( d_t ; c_t )}}
#' \item{\code{dlm_Phi}}{Return the matrix \deqn{\Phi_t = ( T_t ; Z_t )}}
#' \item{\code{dlm_Sigma}}{Return the matrix \deqn{\Sigma = (P_1,  a_1' )}}
#' \item{\code{dlm_states}}{Return the number of states, \eqn{m}}
#' \item{\code{dlm_vars}}{Return the number of variables, \eqn{N}}
#' \item{\code{dlm_obs}}{Return the number of observations; returns 0 if not time-varying}
#'}
NULL

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
  rBind(cBind(HH, HG), cBind(t(HG), GG))
}

dlm_Phi <- function(object, i = NULL) {
  rBind(dlm_T(object, i), dlm_Z(object, i))
}

dlm_Sigma <- function(object) {
  rBind(object@P1, t(object@a1))
}

dlm_delta <- function(object, i = NULL) {
  rBind(dlm_dd(object, i), dlm_cc(object, i))
}

dlm_states <- function(object) nrow(object@T)

dlm_vars <- function(object) nrow(object@Z)

dlm_obs <- function(object) nrow(object@X)
