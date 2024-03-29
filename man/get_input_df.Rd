% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_input_df.R
\name{get_input_df}
\alias{get_input_df}
\title{get_input_df}
\usage{
get_input_df(x, gamma = TRUE, long = FALSE)
}
\arguments{
\item{x}{\code{nutr_input} object simulated with \code{simulate_nutrient_*}.}

\item{gamma}{Logical if TRUE, the sum of gamma (regional) scale will be added.}

\item{long}{Logical if TRUE, \code{data.frame} will be reshaped to long format.}
}
\value{
data.frame
}
\description{
Get input data.frame
}
\details{
Returns a \code{data.frame} with all local nutrient inputs and total nutrients
input per timestep.
}
\examples{
\dontrun{
nutrients_input <- simulate_nutrient_sine(n = 3, max_i = 4380, input_mn = 1,
frequency = 3, noise = 0.5)
get_input_df(nutrients_input)
}

}
