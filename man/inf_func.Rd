% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_from_estimates.R
\name{inf_func}
\alias{inf_func}
\alias{inf_func.lm}
\alias{inf_func.fixest}
\title{inf_func}
\usage{
inf_func(est, ...)

\method{inf_func}{lm}(est, ...)

\method{inf_func}{fixest}(est, ...)
}
\arguments{
\item{est}{model object}

\item{...}{other arguments}
}
\value{
A N x k matrix of influence function where N is the number of
observations and k is the number of moments (parameters in the linear
case).
}
\description{
Extract vector of empirical influence function from regression model
}
\details{
Extracts the empirical influence function from a model
\url{https://en.wikipedia.org/wiki/Robust_statistics#Empirical_influence_function}.
This is useful for residaul bootstrap and for cross-model inference.
In linear models, influence function is given by: (X' W X)^-1 X' W^1/2 y.
}
\examples{
  mod <- fixest::feols(mpg ~ wt, data = mtcars)
  inf_func(mod)
}
