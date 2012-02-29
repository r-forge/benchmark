\name{as.warehouse}
\alias{as.warehouse.mlr.bench.result}
\title{as.warehouse}
\usage{as.warehouse.mlr.bench.result(x, ...)
as.warehouse.array4dim(x, ...)
}
\description{Methods to coerce objects to a benchmark experiment warehouse.}
\details{\code{as.warehouse.mlr.bench.result}: Coerces a \code{bench.result} object from package \code{mlr} to a
\code{\link{warehouse}} object.

\code{as.warehouse.array4dim}: Coerces a four dimensional array (1st: sampling, 2nd: algorithms,
3rd: performance measures, 4th: datasets) to a
\code{\link{warehouse}} object.


}
\value{\code{as.warehouse.mlr.bench.result}: A \code{\link{warehouse}} object

\code{as.warehouse.array4dim}: A \code{\link{warehouse}} object


}
\alias{as.warehouse}
\arguments{\item{x}{A \code{bench.result} object from package \code{mlr}}
\item{...}{Ignored}
}
\alias{as.warehouse.array4dim}

