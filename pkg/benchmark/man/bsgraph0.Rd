\name{bsgraph0}
\alias{bsgraph0}
\title{Benchmark experiment graph.}
\usage{bsgraph0(x, ...)
\method{bsgraph0}{dist}(x, ndists.show=length(sort(unique(x))), edge.col=gray(0.7), edge.lwd=1,
    node.fill, ...)
\method{bsgraph0}{graphNEL}(x, layoutType="neato", ...)
}
\description{Benchmark experiment graph.}
\details{\code{bsgraph0}: The benchmark summary plot takes the individual benchmark
experiment results into account. The y-axis represents the
data sets, the x-axis a podium with as many places as
candidate algorithms.

}
\arguments{\item{x}{The object to plot}
\item{...}{Unused}
\item{ndists.show}{The number of distance levels to show}
\item{edge.col}{The color of edges (one or one for each distance level)}
\item{edge.lwd}{The line width of edges (one or one for each distance level)}
\item{node.fill}{The colors of nodes}
\item{layoutType}{Defines the layout engine}
}
\alias{bsgraph0.dist}
\value{\code{bsgraph0.dist}: The return value of \code{\link{bsgraph0.graphNEL}}

\code{bsgraph0.graphNEL}: Invisible return of the \code{\link[Rgraphviz]{Ragraph}} object


}
\alias{bsgraph0.graphNEL}

