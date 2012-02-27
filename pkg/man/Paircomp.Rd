\name{Paircomp}
\alias{Paircomp}
\title{Infrastructure for pairwise comparisons of algorithm performances...}
\description{Infrastructure for pairwise comparisons of algorithm performances}
\details{\code{Paircomp}: Available \code{TestPaircomp} implementations:
\tabular{rl}{
\code{FriedmanTestPaircomp} \tab Pairwise comparison based on the
non parametric friedman test\cr
\code{LmerTestPaircomp} \tab Pairwise comparison based on a mixed
effects model (function \code{lmer} in package \code{lme4})\cr
\code{PercintTestPaircomp} \tab Pairwise comparison based on the
bootstrap percentile intervals
}

Available \code{PointPaircomp} implementations:
\tabular{rl}{
\code{GenericPointPaircomp} \tab Pairwise comparison based on
point estimates.
}

}
\alias{TestPaircomp}
\alias{PointPaircomp}
\references{See \emph{Eugster and Leisch (2008)} and \emph{Eugster et al. (2008)}
in \code{citation("benchmark")}.}
\seealso{\code{\link{TestProcedure}}}
\alias{FriedmanTestPaircomp}
\alias{LmerTestPaircomp}
\alias{PercintTestPaircomp}
\alias{GenericPointPaircomp}

