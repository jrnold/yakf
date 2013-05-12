#' @include yakf-package.R
#' @include DLM.R
#' @include utilities.R
#' @export dlm_cyclical dlm_cyclical_tau dlm_cyclical_season
NULL

# cyclical transition matrix at time i.
cycl_trans_mat <- function(i, omega) {
  Matrix(c(cos(omega * i), -sin(omega * i), sin(omega * i), cos(omega * i)), 2, 2)
}


#' @name dlm_cyclical
#' @rdname dlm_cyclical
#' @title Create Fourier representation of a cyclical DLM
#'
#' @description This function creates a \code{\linkS4class{DLM}} object for a cyclical model.
#' 
#' @param omega \code{numeric} The frequency.
#' @param q \code{integer} The number of harmonics.
#' @param sigma2 \code{numeric} The observation variance, \eqn{G G' = \sigma^2}.
#' @param HH \code{matrix} The system covariance matrix, \eqn{H H'}. If \code{NULL}, then \eqn{H H' = 0}.
#' @param season \code{logical} If \code{TRUE}, then the period is assumed to be an integer. This results in a special
#' case if the period is even and \code{q = s / 2}.
#' @param ... Arguments passed to \code{dlm_cyclical}.
#' @return An object of class \code{\linkS4class{DLM}}.
dlm_cyclical <- function(omega, q = 1, sigma2 = 1, HH = NULL, a1 = NULL, P1 = NULL, season = FALSE) {
  ## Checks
  q <- as.integer(len_one_arg(q, "q"))
  sigma2 <- len_one_arg(sigma2)
  season <- len_one_arg(season)
  check_positive(q, "q")
  check_positive(omega, "omega")
  check_positive(sigma2, "sigma2", include_zero = TRUE)
  ## body
  ## Special seasonal case in which s is even, q = s / 2
  s <- as.integer((2 * base::pi) / omega)
  if (season && !(s %% 2) && (q == s / 2)) {
    even <- TRUE
    matlist <- c(lapply(seq_len(q - 1), cycl_trans_mat, omega = omega),
                 Matrix(-1, 1, 1))
  } else {
    even <- FALSE
    matlist <- lapply(seq_len(q), cycl_trans_mat, omega = omega)
  }
  T <- do.call(bdiag, matlist)
  m <- nrow(T) # number of states
  if (even) {
    Z <- Matrix(c(rep(c(1, 0), (m - 1) / 2), 1), 1, m)
  } else {
    Z <- Matrix(c(1, 0), 1, m)
  }
  if (is.null(HH)) {
    HH <- Matrix(0, m, m)
  }
  GG <- Matrix(sigma2, 1, 1)
  DLM(T = T, Z = Z, HH = HH, GG = GG, a1 = a1, P1 = P1)
}

#' @rdname dlm_cyclical
#' @aliases dlm_cyclical_tau
#' @param tau \code{numeric} The period, \eqn{\omega = \frac{2 \pi}{\tau}}.
dlm_cyclical_tau <- function(tau, ...) {
  check_positive(omega, "tau")
  omega <- (base::pi * 2) / tau
  dlm_cyclical(omega, ...)
}

#' @rdname dlm_cyclical
#' @aliases dlm_cyclical_season
#' @param s \code{integer} The number of seasons, \eqn{\omega = \frac{2 \pi}{s}}.
dlm_cyclical_season <- function(s, q = floor(s / 2), season = TRUE, ...) {
  if (q > floor(s / 2)) {
    stop("'q' cannot be larger than floor(s / 2)")
  }
  s <- as.integer(len_one_arg(s, "s"))
  check_positive(omega, "s")
  omega <- (base::pi * 2) / s
  dlm_cyclical(omega, q = q, season = TRUE,  ...)
}
