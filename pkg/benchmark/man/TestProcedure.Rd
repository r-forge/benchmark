\docType{data}
\name{TestProcedure}
\alias{FriedmanTest}
\alias{LmerTest}
\alias{PercintTest}
\alias{TestProcedure}
\title{Test procedures infrastructure}
\format{\preformatted{proto object 
 $ requirements:function (., ...)   
 $ pairwiseTest:function (., ...)   
 $ globalTest  :function (., ...)   
 $ new         :function (., ...)   
}}
\usage{
FriedmanTest
  LmerTest
  PercintTest
}
\description{
Test procedures infrastructure
}
\details{
Implemented \code{TestProcedure} and corresponding
\code{TestResult} are: \tabular{rl}{ \code{FriedmanTest}
\tab Test procedure based on the non parametric friedman
test\cr \code{LmerTest} \tab Test procedure based on a
mixed effects model (function \code{lmer} in package
\code{lme4})\cr \code{PercintTest} \tab Test procedure
based on the bootstrap percentile intervals }
}
\references{
See \emph{Eugster and Leisch (2008)} and \emph{Eugster et
al. (2008)} in \code{citation("benchmark")}.
}
\keyword{datasets}

