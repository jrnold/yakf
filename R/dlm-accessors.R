#' @include yakf-package.R
#' @include DLM-class.R
#' @export dlm_T dlm_Z dlm_HH dlm_GG dlm_HG dlm_cc dlm_dd
#' @export dlm_Sigma dlm_Omega dlm_delta dlm_Phi
NULL

#' @name dlm-accessors
#' @rdname dlm-accessors
#' @aliases dlm_cc dlm_dd dlm_T dlm_Z dlm_HH dlm_HG dlm_Omega dlm_Phi dlm_Sigma dlm_delta
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
#' \item{\code{get_cc}}{Return \eqn{c_t} vector}
#' \item{\code{get_dd}}{Return \eqn{d_t} vector}
#' \item{\code{get_T}}{Return \eqn{T_t} matrix}
#' \item{\code{get_Z}}{Return \eqn{Z_t} matrix}
#' \item{\code{get_HH}}{Return \eqn{H_t H_t'} matrix}
#' \item{\code{get_GG}}{Return \eqn{G_t G_t'} matrix}
#' \item{\code{get_HG}}{Return \eqn{H_t G_t'} matrix}
#' \item{\code{get_Omega}}{Return the matrix,
#' \deqn{\Omega_t = ( H_t H_t' ,  H_t G_t' ; G_t H_t' , G_t G_t' )}
#' }
#' \item{\code{get_delta}}{Return the matrix \deqn{\delta_t = ( d_t ; c_t )}}
#' \item{\code{get_Phi}}{Return the matrix \deqn{\Phi_t = ( T_t ; Z_t )}}
#' \item{\code{get_Sigma}}{Return the matrix \deqn{\Sigma = (P_1,  a_1' )}}
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
