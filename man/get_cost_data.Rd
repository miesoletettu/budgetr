% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_cost_data.R
\name{get_cost_data}
\alias{get_cost_data}
\title{Nettokäyttökustannukset toiminnoittain}
\usage{
get_cost_data(year = "*", lazy = TRUE)
}
\arguments{
\item{year}{numero tai vektori. Käyvät arvot ovat 2015 - 2018}
}
\value{
A tibble
}
\description{
Nettokäyttökustannukset toiminnoittain
}
\examples{
cost_data <- get_cost_data() # Nettokäyttökustannukset

}
