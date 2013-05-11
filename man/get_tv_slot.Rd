\name{get_tv_slot}
\alias{dlm_HG}
\alias{dlm_HH}
\alias{dlm_Omega}
\alias{dlm_Phi}
\alias{dlm_Sigma}
\alias{dlm_T}
\alias{dlm_Z}
\alias{dlm_cc}
\alias{dlm_dd}
\alias{dlm_delta}
\alias{get_tv_slot}
\title{Access data from DLM objects

These are some utility functions to access system matrices
from a DLM object. They are particularly useful if the object has
time varying parameters.}
\usage{
  get_tv_slot(object, slot, i)
}
\arguments{
  \item{object}{An object of class \code{DLM}.}

  \item{i}{time}
}
\value{
  A \code{matrix} or \code{numeric} vector.
}
\description{
  rdname dlm-accessors
}
\section{Details}{

}

