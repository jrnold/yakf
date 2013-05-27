\name{MatrixList-class}
\Rdversion{1.1}
\docType{class}
\alias{MatrixList-class}
\alias{\%bd\%,MatrixList,Matrix-method}
\alias{\%bd\%,MatrixList,MatrixList-method}
\alias{\%bd\%,Matrix,MatrixList-method}
\alias{cbind2,MatrixList,Matrix-method}
\alias{cbind2,MatrixList,MatrixList-method}
\alias{cbind2,Matrix,MatrixList-method}
\alias{dim,MatrixList-method}
\alias{Ops,MatrixList,Matrix-method}
\alias{Ops,MatrixList,MatrixList-method}
\alias{Ops,Matrix,MatrixList-method}
\alias{rbind2,MatrixList,Matrix-method}
\alias{rbind2,MatrixList,MatrixList-method}
\alias{rbind2,Matrix,MatrixList-method}

\title{Class \code{"MatrixList"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MatrixList", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{MatrixOrMatrixList}"}, directly.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
}
\section{Methods}{
  \describe{
    \item{\%bd\%}{\code{signature(e1 = "MatrixList", e2 = "Matrix")}: ... }
    \item{\%bd\%}{\code{signature(e1 = "MatrixList", e2 = "MatrixList")}: ... }
    \item{\%bd\%}{\code{signature(e1 = "Matrix", e2 = "MatrixList")}: ... }
    \item{cbind2}{\code{signature(x = "MatrixList", y = "Matrix")}: ... }
    \item{cbind2}{\code{signature(x = "MatrixList", y = "MatrixList")}: ... }
    \item{cbind2}{\code{signature(x = "Matrix", y = "MatrixList")}: ... }
    \item{dim}{\code{signature(x = "MatrixList")}: ... }
    \item{Ops}{\code{signature(e1 = "MatrixList", e2 = "Matrix")}: ... }
    \item{Ops}{\code{signature(e1 = "MatrixList", e2 = "MatrixList")}: ... }
    \item{Ops}{\code{signature(e1 = "Matrix", e2 = "MatrixList")}: ... }
    \item{rbind2}{\code{signature(x = "MatrixList", y = "Matrix")}: ... }
    \item{rbind2}{\code{signature(x = "MatrixList", y = "MatrixList")}: ... }
    \item{rbind2}{\code{signature(x = "Matrix", y = "MatrixList")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("MatrixList")
}
\keyword{classes}
