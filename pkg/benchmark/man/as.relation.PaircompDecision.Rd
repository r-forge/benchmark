\name{as.relation.PaircompDecision}
\alias{as.relation.PaircompDecision}
\alias{relation_is_strict_weak_order}
\title{Relations based on pairwise comparisons}
\usage{
  \method{as.relation}{PaircompDecision} (x,
    verbose = FALSE, ...)

  relation_is_strict_weak_order(x)
}
\arguments{
  \item{x}{A \code{\link{PaircompDecision}} object}

  \item{verbose}{Show information during execution}

  \item{...}{Ignored}
}
\value{
  A \code{\link[relations]{relation}} object
}
\description{
  Infer a \code{\link[relations]{relation}} based on
  pairwise decisions.
}

