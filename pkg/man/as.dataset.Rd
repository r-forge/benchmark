\name{as.dataset}
\alias{as.dataset}
\title{A dataset abstraction to simplify the calculation of dataset...}
\usage{as.dataset(formula, data, ordered.as.factor=TRUE, integer.as.numeric=TRUE)
}
\description{A dataset abstraction to simplify the calculation of dataset
characteristics.}
\value{A proto object with an additional S3 class \code{dataset}}
\arguments{\item{formula}{A symbolic description of the dataset}
\item{data}{The data frame}
\item{ordered.as.factor}{Interpret ordered factors as factors}
\item{integer.as.numeric}{Interpret integer variables as numerics}
}
\examples{data("iris")
ds <- as.dataset(Species ~ ., iris)
ds

str(ds$response())
str(ds$dataparts(c("input", "numeric")))}

