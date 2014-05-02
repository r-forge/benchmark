\name{characterize}
\alias{characterize}
\title{Dataset characterization framework}
\usage{
characterize(x, y, verbose = FALSE, index = NULL, ...)
}
\arguments{
  \item{x}{A \code{\link[=as.dataset]{dataset}} object}

  \item{y}{A \code{DatasetCharacteristics} object; e.g.,
  \code{\link{StatlogCharacteristics}}}

  \item{verbose}{Show information during execution}

  \item{index}{Characterize only a subset}

  \item{...}{Ignored}
}
\value{
The characterization matrix (1 row and as many columns as
characteristics
}
\description{
Implementation of a map/reduce approach to characterize a
dataset with given dataset characteristics.
}
\examples{
data("iris")
  ds <- as.dataset(Species ~ ., iris)
  characterize(ds, StatlogCharacteristics)
}
\references{
See \emph{Eugster et al. (2010)} in
\code{citation("benchmark")}.
}
\seealso{
Other dataset.characterization:
\code{\link{StatlogCharacteristics}};
\code{\link{as.dataset}};
\code{\link{plot.DatasetCharacterization}}
}

