\name{bsplot0}
\alias{bsplot0}
\alias{bsplot0.matrix}
\alias{bsplot0.relation_ensemble}
\title{Benchmark experiment summary plot.}
\usage{
  bsplot0(x, ...)

  \method{bsplot0}{relation_ensemble} (x, stat = NULL,
    ds.order = NULL, alg.order = NULL, ...)

  \method{bsplot0}{matrix} (x, stat = NULL,
    col = structure(seq_len(nrow(x)) + 1, names = rownames(x)),
    ylab = "Datasets", xlab = "Podium", sig.lwd = 4,
    stat.col = NULL, ylab.las = NULL, ...)
}
\arguments{
  \item{x}{A \code{\link[relations]{relation_ensemble}} or
  matrix object}

  \item{stat}{A matrix with statistics to display (rows are
  the algorithms, columns the data sets)}

  \item{ds.order}{Data set order}

  \item{alg.order}{Algorithm order}

  \item{...}{Arguments passed to underlying function}

  \item{col}{Colors of the algorithms}

  \item{xlab}{A title for the x axis}

  \item{ylab}{A title for the y axis}

  \item{sig.lwd}{Line width of the significance sperator
  line}

  \item{stat.col}{Colors of the statistics}

  \item{ylab.las}{\code{las} of the labels of the y axis}
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
  \code{\link{bsgraph0}}, \code{\link{bsgraph0.dist}},
  \code{\link{bsgraph0.graphNEL}},
  \code{\link{densityplot}},
  \code{\link{densityplot.AlgorithmPerformance}},
  \code{\link{stripchart.AlgorithmPerformance}}
}

