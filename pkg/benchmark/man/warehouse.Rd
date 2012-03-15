\name{warehouse}
\alias{AlgorithmPerformance}
\alias{DatasetBasisCharacterization}
\alias{DatasetCharacterization}
\alias{TestResult}
\alias{warehouse}
\title{Benchmark experiment warehouse}
\usage{
  warehouse(datasets, B, algorithms = NULL,
    performances = NULL, characteristics = NULL,
    tests = NULL)
}
\arguments{
  \item{datasets}{Names of the datasets}

  \item{B}{Number of benchmark runs}

  \item{algorithms}{Names of the candidate algorithms}

  \item{performances}{Names of the performance measures}

  \item{characteristics}{Names of the dataset
  characteristics}

  \item{tests}{Names of the monitored test measures}
}
\value{
  Proto object with different views (see Details).
}
\description{
  \code{warehouse} is the constructor function for a
  benchmark experiment warehouse.
}
\details{
  A benchmark experiment warehouse collects all data during
  the execution of a benchmark experiment; see
  \code{\link{benchmark}}.  Different views (based on the
  collected data) provide cleaned parts of the data for
  further analyses.

  Implemented views: \enumerate{ \item
  \code{viewAlgorithmPerformance()}: returns a data frame
  (S3 class \code{AlgorithmPerformance}) with columns
  \code{samples, datasets, algorithms, performances}
  (factors with the corresponding levels) and the column
  \code{value} with the corresponding performance value.

  \item \code{viewDatasetCharacterization()}: returns a
  data frame (S3 class \code{DatasetCharacterization}) with
  columns \code{samples, datasets, characteristics, value}.

  \item \code{viewDatasetBasisCharacterization()}: returns
  a data frame (S3 class
  \code{DatasetBasisCharacterization}) with columns
  \code{datasets, characteristics, value}.

  \item \code{viewTestResult()}: returns a data frame (S3
  class \code{TestResult}) with columns \code{samples,
  datasets, tests, value}.  }
}
\seealso{
  \code{\link{benchmark}}, \code{\link{as.warehouse}}
}

