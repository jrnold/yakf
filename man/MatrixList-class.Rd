\docType{class}
\name{MatrixList-class}
\alias{MatrixList-class}
\alias{Ops,Matrix,MatrixList-method}
\alias{Ops,MatrixList,Matrix-method}
\alias{Ops,MatrixList,MatrixList-method}
\alias{\%bd\%,Matrix,MatrixList-method}
\alias{\%bd\%,MatrixList,Matrix-method}
\alias{\%bd\%,MatrixList,MatrixList-method}
\alias{cbind2,Matrix,MatrixList-method}
\alias{cbind2,MatrixList,Matrix-method}
\alias{cbind2,MatrixList,MatrixList-method}
\alias{dim,MatrixList-method}
\alias{rbind2,Matrix,MatrixList-method}
\alias{rbind2,MatrixList,Matrix-method}
\alias{rbind2,MatrixList,MatrixList-method}
\title{List of Matrix objects}
\description{
  A list of \code{\linkS4class{Matrix}} objects, which must
  all have the same dimensions.
}
\section{Extends}{
  \describe{ \item{\code{list}}{Directly.} }
}

\section{Methods}{
  Many of these methods simply apply a function elementwise
  to each matrix in the object, and work for both
  \code{"MatrixList"} objects and \code{"Matrix"} objects.

  \describe{ \item{\%bd\%}{\code{signature(e1 =
  "MatrixList", e2 = "Matrix")}: Block diagonal of
  \code{e1} and \code{e2}.}
  \item{\%bd\%}{\code{signature(e1 = "MatrixList", e2 =
  "MatrixList")}: Block diagonal of \code{e1} and
  \code{e2}.} \item{\%bd\%}{\code{signature(e1 = "Matrix",
  e2 = "MatrixList")}: Block diagonal of \code{e1} and
  \code{e2}.} \item{cbind2}{\code{signature(x =
  "MatrixList", y = "Matrix")}: Bind columns of \code{e1}
  and \code{e2}.} \item{cbind2}{\code{signature(x =
  "MatrixList", y = "MatrixList")}: Bind columns of
  \code{e1} and \code{e2}.} \item{cbind2}{\code{signature(x
  = "Matrix", y = "MatrixList")}: Bind columns of \code{e1}
  and \code{e2}.} \item{dim}{\code{signature(x =
  "MatrixList")} : Dimensions of the matrix elements in the
  object. Use \code{length} to the get the number of
  matrices.} \item{Ops}{\code{signature(e1 = "MatrixList",
  e2 = "Matrix")}: Operators. These are applied elementwise
  to \code{e1} and \code{e2}.}
  \item{Ops}{\code{signature(e1 = "MatrixList", e2 =
  "MatrixList")}: Operators. These are applied elementwise
  to \code{e1} and \code{e2}.}
  \item{Ops}{\code{signature(e1 = "Matrix", e2 =
  "MatrixList")}: Operators. These are applied elementwise
  to \code{e1} and \code{e2}.}
  \item{rbind2}{\code{signature(x = "MatrixList", y =
  "Matrix")}: Bind rows of \code{e1} and \code{e2}.}
  \item{rbind2}{\code{signature(x = "MatrixList", y =
  "MatrixList")}: Bind rows of \code{e1} and \code{e2}.}
  \item{rbind2}{\code{signature(x = "Matrix", y =
  "MatrixList")}: Bind rows of \code{e1} and \code{e2}.} }
}
\examples{
library("Matrix")
# create a Matrix List of three 2 x 2 matrices
foo <- MatrixList(Matrix(1, 2, 2), Matrix(2, 2, 2), Matrix(3, 2, 2))

# create another list
bar <- MatrixList(replicate(length(foo), Matrix(5, 2, 2)))
# Some methods (defined for both MatrixList objects, and
# MatrixList, Matrix object combinations
foo - bar
foo + bar
foo[[1]] + bar
cBind(foo, bar)
rBind(foo, bar)
# block diagonal
}

