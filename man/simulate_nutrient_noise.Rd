% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_nutrient_noise.R
\name{simulate_nutrient_noise}
\alias{simulate_nutrient_noise}
\title{simulate_nutrient_noise}
\usage{
simulate_nutrient_noise(
  n,
  max_i,
  frequency = 0,
  input_mn = 0,
  noise = 0,
  n_noise = 3,
  verbose = TRUE
)
}
\arguments{
\item{n}{Integer with number of metaecosystems to setup.}

\item{max_i}{Integer with maximum number of simulation time steps.}

\item{frequency}{Numeric frequency.}

\item{input_mn}{Numeric with mean input amount and frequency.}

\item{noise}{Numeric with noise function sd.}

\item{n_noise}{Integer with number of noise functions}

\item{verbose}{If TRUE, progress reports are printed.}
}
\value{
nutr_input
}
\description{
Simulate nutrient input.
}
\details{
Simulating nutrient input based on Perlin noise.
}
\examples{
nutrients_input <- simulate_nutrient_noise(n = 3, max_i = 4380, frequency = 5,
input_mn = 1, noise = 0.5)
plot(nutrients_input)

}
