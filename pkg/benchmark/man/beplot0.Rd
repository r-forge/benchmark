\name{beplot0}
\alias{beplot0}
\alias{beplot0.AlgorithmPerformance}
\alias{beplot0.matrix}
\title{Benchmark experiment plot}
\usage{
  beplot0(x, ...)

  \method{beplot0}{AlgorithmPerformance} (x, xlab = NULL,
    ylab = NULL, lines.show = FALSE, lines.alpha = 0.2,
    lines.lwd = 1, lines.col = col, dots.pch = 19,
    dots.cex = 1, places.lty = 2, places.col = 1,
    legendfn = function(algs, cols) {
        legend("topleft", algs, lwd = 1, col = cols, bg = "white")
    }, ...)

  \method{beplot0}{matrix} (x, col = 1:ncol(x),
    xlab = NULL, ylab = NULL, lines.show = FALSE,
    lines.alpha = 0.2, lines.lwd = 1, lines.col = col,
    dots.pch = 19, dots.cex = 1, places.lty = 2,
    places.col = 1, legendfn = function(algs, cols) {
        legend("topleft", algs, lwd = 1, col = cols, bg = "white")
    }, ...)
}
\arguments{
  \item{x}{A matrix or
  \code{\link[=warehouse]{AlgorithmPerformance}} object}

  \item{xlab}{A title for the x axis}

  \item{ylab}{A title for the y axis}

  \item{lines.show}{Connect dots of same benchmark runs}

  \item{lines.col}{Line color}

  \item{lines.alpha}{Alpha value of the line color}

  \item{lines.lwd}{Line width}

  \item{dots.pch}{Dot symbol}

  \item{dots.cex}{Dot symbol expansion}

  \item{places.lty}{Type of separator line between podium
  places}

  \item{places.col}{Color of separator line between podium
  places}

  \item{legendfn}{Function which draws a legend}

  \item{...}{Ignored}

  \item{col}{Colors}
}
\value{
  Return value of underlying \code{beplot0.matrix};
  currently undefined
}
\description{
  The benchmark experiment plot visualizes each benchmark
  experiment run. The x-axis is a podium with as many
  places as algorithms. For each benchmark run, the
  algorithms are sorted according to their performance
  values and a dot is drawn on the corresponding place. To
  visualize the count of an algorithm on a specific
  position, a bar plot is shown for each of podium places.
}
\references{
  See \emph{Eugster and Leisch (2008)} and \emph{Eugster et
  al. (2008)} in \code{citation("benchmark")}.
}
\seealso{
  Other algperf.visualization:
  \code{\link{boxplot.AlgorithmPerformance}},
  \code{\link{bsgraph0}}, \code{\link{bsgraph0.dist}},
  \code{\link{bsgraph0.graphNEL}}, \code{\link{bsplot0}},
  \code{\link{bsplot0.matrix}},
  \code{\link{bsplot0.relation_ensemble}},
  \code{\link{densityplot}},
  \code{\link{densityplot.AlgorithmPerformance}},
  \code{\link{stripchart.AlgorithmPerformance}}
}

