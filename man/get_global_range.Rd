% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_global_range.R
\name{get_global_range}
\alias{get_global_range}
\title{get_global_range}
\usage{
get_global_range(result, value)
}
\arguments{
\item{result}{List with \code{meta_rn} objects simulated with \code{simulate_nutr_input_*}.}

\item{value}{Character with coloumn of seafloor to summarize.}
}
\value{
vector
}
\description{
Get global range
}
\details{
Returns the minimum and maximum of selected seafloor value of several model run results
created with \code{run_simulation_meta}.
}
\examples{
\dontrun{
get_global_range(result = list(result_rand, result_rand), value = "ag_biomass")
}

}
