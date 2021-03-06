% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upriver.R
\name{timings}
\alias{timings}
\title{Calculate run timings}
\usage{
timings(location, arrival, parameters, arrival_position = 0)
}
\arguments{
\item{location}{(numeric) The location at which to calculate to run timing.}

\item{arrival}{(data.frame) A data.frame of daily arrivals by proportion.
See details.}

\item{parameters}{(list) See \code{\link{positions}} for details.}

\item{arrival_position}{(numeric) Defaults 0. Where to start movement.}
}
\description{
Calculates run timing given a set of parameters.
}
\details{
\code{arrival} should be a \code{data.frame} describing a set of daily
arrivals of fish at \code{arrival_position} as a series of daily proportions.
It should have a column \code{day} and a corresponding column
\code{proportion}, which should sum to one.
}
\examples{
timings(1000,
        data.frame(day=0:40, proportion = dnorm(-20:20, 0, 5)/sum(dnorm(-20:20, 0, 5))),
        list(rates = 50, distances = 1000))
}
