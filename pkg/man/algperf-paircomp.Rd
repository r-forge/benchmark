\name{algperf-paircomp}
\alias{paircomp}
\title{Pairwise comparison of algorithm performances...}
\usage{paircomp(x, family, type=c("<", "="), ...)
\method{as.relation}{PaircompDecision}(x, verbose=FALSE, ...)
relation_is_strict_weak_order(x)
}
\description{Pairwise comparison of algorithm performances}
\value{\code{paircomp}: A \code{PaircompDecision} object; a list with the
elements:
\tabular{rl}{
\code{decision} \tab The incidence matrix representing the
pairwise comparisons\cr
\code{type} \tab The decision type\cr
\code{base} \tab A list with information on the decision base
}

\code{as.relation.PaircompDecision}: A \code{relation} object


}
\alias{PaircompDecision}
\references{See \emph{Eugster and Leisch (2008)} and \emph{Eugster et al. (2008)}
in \code{citation("benchmark")}.}
\arguments{\item{x}{An \code{\link{AlgorithmPerformance}} object}
\item{family}{A \code{\link{Paircomp}} object}
\item{type}{Draw strict or indifference decision}
\item{...}{Ignored}
\item{verbose}{Show information during execution}
}
\alias{as.relation.PaircompDecision}
\alias{relation_is_strict_weak_order}

