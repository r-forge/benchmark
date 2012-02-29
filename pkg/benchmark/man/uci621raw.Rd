\name{uci621raw}
\docType{data}
\alias{uci621raw}
\alias{uci621rel}
\title{uci621 benchmark experiment results}
\description{
Misclassification error and computation time on the data set
21 UCI Machine Learning Repository data sets of the candidate algorithms
\code{lda}, \code{rf}, \code{knn}, \code{rpart}, \code{svm} and
\code{nnet}.

\code{uci621rel} are precomputed relations.

Used in \code{demo("lsbenchplot-cs621")}.
}
\usage{uci621raw}
\format{An array of dimension 250 x 6 x 2 x 1 (sampling x algorithms x
  performances x data sets).}
\references{
Manuel J. A. Eugster, Torsten Hothorn and Friedrich Leisch. "Exploratory
and Inferential Analysis of Benchmark Experiments", Technical Report 30,
Department of Statistics, Ludwig-Maximilians-Universitaet Muenchen,
Germany. 2008. http://epub.ub.uni-muenchen.de/4134/
}
\keyword{datasets}
