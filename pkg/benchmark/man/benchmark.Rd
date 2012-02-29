\name{benchmark}
\alias{benchmark}
\title{Benchmark experiment execution}
\usage{benchmark(datasets, sampling, algorithms, performances, characteristics, test,
    test.burnin=3, verbose=TRUE)
}
\description{Function to execute benchmark experiments and collect all data the
package can analyze. For more sophisticated benchmark experiments
we suggest the usage of the \code{mlr} package.}
\value{A \code{\link{warehouse}} object}
\seealso{\code{\link{warehouse}}, \code{\link{as.warehouse}},
\code{\link{benchmark-sampling}}, \code{\link{benchmark-comptime}}}
\arguments{\item{datasets}{List of data.frames}
\item{sampling}{Sampling function, see \code{\link{benchmark-sampling}}.}
\item{algorithms}{List of algorithms; i.e., functions which take
a model formula and a data.frame to fit a model. Note that a
\code{\link[stats]{predict}} function must be defined as well.}
\item{performances}{List of performance measure functions; i.e.,
functions with arguments \code{yhat} and \code{y}. See, e.g.,
\code{\link{benchmark-comptime}}.}
\item{characteristics}{\code{\link{DatasetCharacteristics}} object}
\item{test}{\code{\link{TestProcedure}} object}
\item{test.burnin}{Number of burn-in replications}
\item{verbose}{Show information during execution}
}

