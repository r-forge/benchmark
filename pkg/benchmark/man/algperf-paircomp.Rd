\name{paircomp}
\alias{FriedmanTestPaircomp}
\alias{GenericPointPaircomp}
\alias{LmerTestPaircomp}
\alias{PaircompDecision}
\alias{PercintTestPaircomp}
\alias{PointPaircomp}
\alias{TestPaircomp}
\alias{paircomp}
\title{Pairwise comparison of algorithm performances}
\usage{
paircomp(x, family, type = c("<", "="), ...)
}
\arguments{
  \item{x}{An \code{\link{AlgorithmPerformance}} object}

  \item{family}{A \code{Paircomp} object; see details
  section}

  \item{type}{Draw strict or indifference decision}

  \item{...}{Ignored}
}
\value{
A \code{PaircompDecision} object; a list with the elements:
\tabular{rl}{ \code{decision} \tab The incidence matrix
representing the pairwise comparisons\cr \code{type} \tab
The decision type\cr \code{base} \tab A list with
information on the decision base }
}
\description{
Pairwise comparison of algorithm performances
}
\details{
Available \code{TestPaircomp} implementations:
\tabular{rl}{ \code{FriedmanTestPaircomp} \tab Pairwise
comparison based on the non parametric friedman test\cr
\code{LmerTestPaircomp} \tab Pairwise comparison based on a
mixed effects model (function \code{lmer} in package
\code{lme4})\cr \code{PercintTestPaircomp} \tab Pairwise
comparison based on the bootstrap percentile intervals }

Available \code{PointPaircomp} implementations:
\tabular{rl}{ \code{GenericPointPaircomp} \tab Pairwise
comparison based on point estimates.  }
}
\references{
See \emph{Eugster and Leisch (2008)} and \emph{Eugster et
al. (2008)} in \code{citation("benchmark")}.
}

