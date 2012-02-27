\name{warehouse}
\alias{warehouse}
\title{Benchmark experiment warehouse.}
\usage{warehouse(datasets, B, algorithms, performances, characteristics, tests)
}
\description{Benchmark experiment warehouse.}
\details{\code{warehouse} is the constructor function for a benchmark experiment
warehouse.

A benchmark experiment warehouse collects all data during a benchmark
experiment (\code{\link{benchmark}} is a proof-of-concept implementation).
Different views (based on the collected data) provide cleaned parts
of the data for further analyses.

Implemented views:
\enumerate{
\item \code{viewAlgorithmPerformance()}: returns a data frame (S3
class \code{AlgorithmPerformance}) with columns \code{samples,
datasets, algorithms, performances} (factors with the corresponding
levels) and the column \code{value} with the corresponding
performance value.

\item \code{viewDatasetCharacterization()}: returns a data frame
(S3 class \code{DatasetCharacterization}) with columns \code{samples,
datasets, characteristics, value}.

\item \code{viewDatasetBasisCharacterization()}: returns a data
frame (S3 class \code{DatasetBasisCharacterization}) with columns
\code{datasets, characteristics, value}.

\item \code{viewTestResult()}: returns a data frame (S3 class
\code{TestResult}) with columns \code{samples, datasets, tests, value}.
}}
\value{Proto object with different views (see Details).}
\seealso{\code{\link{benchmark}}, \code{\link{as.warehouse}}}
\alias{AlgorithmPerformance}
\alias{DatasetCharacterization}
\alias{DatasetBasisCharacterization}
\alias{TestResult}
\arguments{\item{datasets}{Names of the datasets}
\item{B}{Number of benchmark runs}
\item{algorithms}{Names of the candidate algorithms}
\item{performances}{Names of the performance measures}
\item{characteristics}{Names of the dataset characteristics}
\item{tests}{Names of the monitored test measures}
}

