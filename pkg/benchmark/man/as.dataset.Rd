\name{as.dataset}
\alias{as.dataset}
\title{Dataset abstraction}
\usage{
  as.dataset(formula, data, ordered.as.factor = TRUE,
    integer.as.numeric = TRUE)
}
\arguments{
  \item{formula}{A symbolic description of the dataset}

  \item{data}{The data frame}

  \item{ordered.as.factor}{Interpret ordered factors as
  factors}

  \item{integer.as.numeric}{Interpret integer variables as
  numerics}
}
\value{
  A proto object with an additional S3 class \code{dataset}
}
\description{
  Dataset abstraction to simplify characterization.
}
\examples{
data("iris")
  ds <- as.dataset(Species ~ ., iris)
  ds

  str(ds$response())
  str(ds$dataparts(c("input", "numeric")))
}
\seealso{
  Other dataset.characterization:
  \code{\link{characterize}},
  \code{\link{plot.DatasetCharacterization}},
  \code{\link{StatlogCharacteristics}}
}

