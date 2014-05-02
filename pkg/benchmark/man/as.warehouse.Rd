\name{as.warehouse.mlr.bench.result}
\alias{as.warehouse}
\alias{as.warehouse.array4dim}
\alias{as.warehouse.mlr.bench.result}
\title{Coerce objects to benchmark experiment warehouse}
\usage{
as.warehouse.mlr.bench.result(x, ...)

as.warehouse.array4dim(x, ...)
}
\arguments{
  \item{x}{An object to coerce}

  \item{...}{Ignored}
}
\value{
A \code{\link{warehouse}} object
}
\description{
Coerce objects to benchmark experiment warehouse
}
\details{
\code{as.warehouse.mlr.bench.result}: Coerces a
\code{bench.result} object from package \code{mlr} to a
\code{\link{warehouse}} object.

\code{as.warehouse.array4dim}: Coerces a four dimensional
array (1st: sampling, 2nd: algorithms, 3rd: performance
measures, 4th: datasets) to a \code{\link{warehouse}}
object.
}

