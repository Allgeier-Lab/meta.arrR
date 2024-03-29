% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_probability.R
\name{calc_probability}
\alias{calc_probability}
\title{calc_probability}
\usage{
calc_probability(
  metasyst,
  lambda = 1,
  diag_value = NA,
  full = TRUE,
  dist = FALSE
)
}
\arguments{
\item{metasyst}{\code{meta_syst} object simulated with \code{setup_meta}.}

\item{lambda}{Distance decay parameter.}

\item{diag_value}{Numeric with value to be used on diagonal.}

\item{full}{Logical if full matrix should be returned.}

\item{dist}{Logical if TRUE list is returned.}
}
\value{
matrix
}
\description{
Calculate probability
}
\details{
Calculate probability matrix for local ecosystems based on distances between them
using a negative exponential function I = exp(-d*lambda). If \code{lambda=0},
movement across metaecosystems is random.

If code{dist=TRUE}, a list with the probabilities and the distance between local
ecosystems is returned.
}
\examples{
\dontrun{
calc_probability(metasyst)
}

}
\references{
https://en.wikipedia.org/wiki/Exponential_decay

https://en.wikipedia.org/wiki/Distance_decay

Nekola, J.C. and White, P.S., 1999. The distance decay of similarity in biogeography
and ecology. Journal of biogeography, 26(4), pp.867-878.
}
