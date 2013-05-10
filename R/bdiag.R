#' Create Block Diagonal Matrix
#'
#' @parm ... \code{matrix} object to form the diagonal blocks.
#' @param offdiag value used to fill in the off-diagonal elements.
#' @return An object of class \code{matrix} with the matrices from \code{...}
#' along the diagonal, and \code{offdiag} for the entries off the block diagonal.
bdiag <- function(..., offdiag = 0) {
  x <- lapply(list(...), as.matrix)
  n <- length(x)
  x_rows <- sapply(x, nrow)
  x_rstart <- cumsum(c(1, x_rows[1:(n - 1)]))
  x_rend <- cumsum(x_rows)
  x_cols <- sapply(x, ncol)
  x_cstart <- cumsum(c(1, x_cols[1:(n - 1)]))
  x_cend <- cumsum(x_cols)
  total_col <- sum(x_cols)
  total_row <- sum(x_rows)
  y <- matrix(offdiag, total_row, total_col)
  for (i in seq_along(x)) {
    y[x_rstart[i]:x_rend[i], x_cstart[i]:x_cend[i]] <- x[[i]]
  }
  y
}
