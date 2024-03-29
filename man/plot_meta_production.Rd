% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_meta_production.R
\name{plot_meta_production}
\alias{plot_meta_production}
\title{plot_meta_production}
\usage{
plot_meta_production(result, lag = TRUE)
}
\arguments{
\item{result}{\code{meta_rn} object simulated with \code{run_simulation_meta}.}

\item{lag}{Logical if TRUE, the difference to the previous timestep is returned.}
}
\value{
ggplot
}
\description{
Plot local abundance.
}
\details{
Plot the production per meta ecosystem.
}
\examples{
\dontrun{
plot_meta_production(result = result_rand)
}

}
