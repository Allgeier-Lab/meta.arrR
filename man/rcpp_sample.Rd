% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{rcpp_sample}
\alias{rcpp_sample}
\title{rcpp_sample}
\usage{
rcpp_sample(x, probs)
}
\arguments{
\item{x}{NumericVector with values.}

\item{probs}{NumericVector with relative probabilities.}
}
\value{
double
}
\description{
Rcpp sample vector
}
\details{
Samples one value from provided vector with given probabilities. The probabilities
must sum up to sum(probs) == 1.
}
\references{
How to use time-based seed based on <http://www.cplusplus.com/reference/algorithm/shuffle/>
}
\keyword{internal}
