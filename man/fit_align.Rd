% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_align.R
\name{fit_align}
\alias{fit_align}
\title{fit_align}
\usage{
fit_align(x, y, fun = function(par, x) par[1] + (par[2] * x), ...)
}
\arguments{
\item{x}{A first time-series \code{vector}, to use a reference when
warping \code{y}.}

\item{y}{A second time-series \code{vector}, to warp align with \code{x}.}

\item{fun}{A \code{function} describing the time-offsetting model to be
applied.}

\item{...}{Other arguments, currently ignored.}
}
\description{
Time warp a data-series using a time-offsetting model.
}
\author{
Karl Ropkins
}