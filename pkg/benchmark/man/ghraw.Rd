\name{ghraw}
\docType{data}
\alias{ghraw}
\alias{ghrel}
\title{Grasshopper domain benchmark experiment results}
\description{
Misclassification error and various other performance measures on the
data set a domain of 33 data sets with information if a specific
grasshopper species is available in a territory or not. The candidate
algorithms are \code{lda}, \code{rf}, \code{knn}, \code{rpart}, \code{svm} and
\code{naiveBayes}.

\code{ghrel} are precomputed relations.

Used in \code{demo("lsbenchplot-gh")}.
}
\usage{ghraw}
\format{A data frame with 99000 observations and 5 variables
  (\code{alg}, \code{samp}, \code{perf}, \code{value}, \code{ds}).
}
\keyword{datasets}
