\name{boxplot.AlgorithmPerformance}
\alias{boxplot.AlgorithmPerformance}
\alias{densityplot}
\alias{densityplot.AlgorithmPerformance}
\alias{stripchart.AlgorithmPerformance}
\title{Common visualizations of algorithm performances}
\usage{
  \method{boxplot}{AlgorithmPerformance} (x,
    order.by = median, order.performance = 1,
    dependence.show = c("outliers", "all", "none"),
    dependence.col = alpha("black", 0.1), ...)

  densityplot(x, ...)

  \method{densityplot}{AlgorithmPerformance} (x, ...)

  \method{stripchart}{AlgorithmPerformance} (x,
    order.by = median, order.performance = 1,
    dependence.show = c("none", "all"),
    dependence.col = alpha("black", 0.1), ...)
}
\arguments{
  \item{x}{An \code{\link{AlgorithmPerformance}} object}

  \item{order.by}{Function like \code{\link{mean}},
  \code{\link{median}}, or \code{\link{max}} to calculate a
  display order of the algorithms; or \code{NULL} for no
  specific order.}

  \item{order.performance}{Name or index of the reference
  performance measure to calculate the order.}

  \item{dependence.show}{Show dependence of observations
  for all, none or outlier observations.}

  \item{dependence.col}{Color of the dependence line.}

  \item{...}{Ignored.}
}
\value{
  A \code{\link[ggplot2]{ggplot}} object.
}
\description{
  Common visualizations of algorithm performances
}
\seealso{
  Other algperf.visualization: \code{\link{beplot0}},
  \code{\link{beplot0.AlgorithmPerformance}},
  \code{\link{beplot0.matrix}}, \code{\link{bsgraph0}},
  \code{\link{bsgraph0.dist}},
  \code{\link{bsgraph0.graphNEL}}, \code{\link{bsplot0}},
  \code{\link{bsplot0.matrix}},
  \code{\link{bsplot0.relation_ensemble}}
}

