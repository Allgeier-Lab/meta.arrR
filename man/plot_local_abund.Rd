% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_local_abund.R
\name{plot_local_abund}
\alias{plot_local_abund}
\title{plot_local_abund}
\usage{
plot_local_abund(result)
}
\arguments{
\item{result}{\code{meta_rn} object simulated with \code{run_simulation_meta}.}
}
\value{
ggplot
}
\description{
Plot local abundance.
}
\details{
The resulting plot describes the number of individuals within each local metaecosystem
over time steps. Additional, the minimum, maximum and mean of all metaecosystems are
included.
}
\examples{
\dontrun{
plot_local_abund(result = result_rand)
}

}
