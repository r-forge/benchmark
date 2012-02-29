\name{bsplot0}
\alias{bsplot0}
\title{Benchmark experiment summary plot.}
\usage{bsplot0(x, ...)
\method{bsplot0}{relation_ensemble}(x, stat, ds.order, alg.order, ...)
\method{bsplot0}{matrix}(x, stat, col=structure(seq_len(nrow(x)) + 1, names = rownames(x)),
    ylab="Datasets", xlab="Podium", sig.lwd=4, stat.col, ylab.las, ...)
}
\description{Benchmark experiment summary plot.}
\details{\code{bsplot0}: The benchmark summary plot takes the individual benchmark
experiment results into account. The y-axis represents the
data sets, the x-axis a podium with as many places as
candidate algorithms.

}
\arguments{\item{x}{The object to plot.}
\item{...}{Unused}
\item{stat}{A matrix with statistics to display (rows are
the algorithms, columns the data sets)}
\item{ds.order}{Data set order}
\item{alg.order}{Algorithm order}
\item{col}{Colors of the algorithms}
\item{xlab}{A title for the x axis}
\item{ylab}{A title for the y axis}
\item{sig.lwd}{Line width of the significance sperator line}
\item{stat.col}{Colors of the statistics}
\item{ylab.las}{\code{las} of the labels of the y axis}
}
\alias{bsplot0.relation_ensemble}
\alias{bsplot0.matrix}

