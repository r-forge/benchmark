\name{bsgraph0}
\alias{bsgraph0}
\alias{bsgraph0.dist}
\alias{bsgraph0.graphNEL}
\title{Benchmark experiment graph}
\usage{
  bsgraph0(x, ...)

  \method{bsgraph0}{dist} (x,
    ndists.show = length(sort(unique(x))),
    edge.col = gray(0.7), edge.lwd = 1, node.fill = NULL,
    ...)

  \method{bsgraph0}{graphNEL} (x, layoutType = "neato",
    ...)
}
\arguments{
  \item{x}{A \code{\link{dist}} or
  \code{\link[graph]{graphNEL-class}} object}

  \item{ndists.show}{The number of distance levels to show}

  \item{edge.col}{The color of edges (one or one for each
  distance level)}

  \item{edge.lwd}{The line width of edges (one or one for
  each distance level)}

  \item{node.fill}{The colors of nodes}

  \item{...}{Arguments passed to underlying function}

  \item{layoutType}{Defines the layout engine}
}
\value{
  The return value of \code{\link{bsgraph0.graphNEL}}
}
\description{
  The benchmark summary plot takes the individual benchmark
  experiment results into account. The y-axis represents
  the data sets, the x-axis a podium with as many places as
  candidate algorithms.
}
\seealso{
  Other algperf.visualization: \code{\link{beplot0}},
  \code{\link{beplot0.AlgorithmPerformance}},
  \code{\link{beplot0.matrix}},
  \code{\link{boxplot.AlgorithmPerformance}},
  \code{\link{bsplot0}}, \code{\link{bsplot0.matrix}},
  \code{\link{bsplot0.relation_ensemble}},
  \code{\link{densityplot}},
  \code{\link{densityplot.AlgorithmPerformance}},
  \code{\link{stripchart.AlgorithmPerformance}}
}

