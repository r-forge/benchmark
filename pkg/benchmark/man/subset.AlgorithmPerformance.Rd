\name{subset.AlgorithmPerformance}
\alias{subset.AlgorithmPerformance}
\title{Return subsets of \code{AlgorithmPerformance} objects}
\usage{
\method{subset}{AlgorithmPerformance}(x, subset, ...)
}
\arguments{
  \item{x}{An \code{\link{AlgorithmPerformance}} object}

  \item{subset}{Logical expression indicating rows to keep}

  \item{...}{Passed to the underlying
  \code{\link{subset.data.frame}} call}
}
\value{
An \code{\link{AlgorithmPerformance}} object with just the
selected observations
}
\description{
Return subsets of \code{AlgorithmPerformance} objects
}

