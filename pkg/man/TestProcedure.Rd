\name{TestProcedure}
\alias{TestProcedure}
\title{Infrastructure for test procedures.}
\description{Infrastructure for test procedures.}
\details{\code{TestProcedure}: Available \code{TestProcedure} and corresponding \code{TestResult}
implementations:
\tabular{rl}{
\code{FriedmanTest} \tab Test procedure based on the
non parametric friedman test\cr
\code{LmerTest} \tab Test procedure based on a mixed
effects model (function \code{lmer} in package \code{lme4})\cr
\code{PercintTest} \tab Test procedure based on the
bootstrap percentile intervals
}

}
\references{See \emph{Eugster and Leisch (2008)} and \emph{Eugster et al. (2008)}
in \code{citation("benchmark")}.}
\alias{FriedmanTest}
\alias{LmerTest}
\alias{PercintTest}

