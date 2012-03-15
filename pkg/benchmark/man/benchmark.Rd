\name{benchmark}
\alias{benchmark}
\title{Benchmark experiment setup and execution}
\usage{
  benchmark(datasets, sampling, algorithms = NULL,
    performances = NULL, characteristics = NULL,
    test = NULL, test.burnin = 3, verbose = TRUE)
}
\arguments{
  \item{datasets}{List of data.frames}

  \item{sampling}{Sampling function, see
  \code{\link{benchmark-sampling}}.}

  \item{algorithms}{List of algorithms; i.e., functions
  which take a model formula and a data.frame to fit a
  model. Note that a \code{\link[stats]{predict}} function
  must be defined as well.}

  \item{performances}{List of performance measure
  functions; i.e., functions with arguments \code{yhat} and
  \code{y}. See, e.g., \code{\link{benchmark-comptime}}.}

  \item{characteristics}{\code{DatasetCharacteristics}
  object; e.g., \code{\link{StatlogCharacteristics}}}

  \item{test}{\code{\link{TestProcedure}} object}

  \item{test.burnin}{Number of burn-in replications}

  \item{verbose}{Show information during execution}
}
\value{
  A \code{\link{warehouse}} object
}
\description{
  Function to execute benchmark experiments and to collect
  all data the package can analyze. For more sophisticated
  benchmark experiments we suggest the usage of the
  \code{mlr} package.
}
\seealso{
  \code{\link{warehouse}}, \code{\link{as.warehouse}},
  \code{\link{benchmark-sampling}},
  \code{\link{benchmark-comptime}}
}

