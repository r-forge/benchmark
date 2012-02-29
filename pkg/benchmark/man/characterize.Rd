\name{characterize}
\alias{characterize}
\title{Dataset characterization framework}
\usage{characterize(x, y, verbose=FALSE, index, ...)
}
\description{Implements a map/reduce approach to characterize a dataset with
given dataset characteristics.}
\value{The characterization matrix (1 row and as many columns as
characteristics}
\seealso{\code{\link{datachar-visualization}}}
\references{See \emph{Eugster et al. (2010)} in \code{citation("benchmark")}.}
\arguments{\item{x}{A \code{\link[=as.dataset]{dataset}} object}
\item{y}{A \code{\link{DatasetCharacteristics}} object}
\item{verbose}{Show information during execution}
\item{index}{Characterize only a subset}
\item{...}{Ignored}
}
\examples{data("iris")
ds <- as.dataset(Species ~ ., iris)
characterize(ds, StatlogCharacteristics)}

