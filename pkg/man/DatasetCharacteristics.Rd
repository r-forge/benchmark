\name{DatasetCharacteristics}
\alias{DatasetCharacteristics}
\title{Dataset characteristics.}
\description{Dataset characteristics.}
\details{\code{DatasetCharacteristics}: "Abstract" proto object with the methods:
\tabular{rl}{
\code{requirements()} \tab Ensures that all needed packages
are available\cr
\code{map()} \tab Returns the list of map functions\cr
\code{reduce()} \tab Returns the list of reduce functions
}
An implementation has to override these methods.

Available implementations:
\tabular{rlll}{
\code{StatlogCharacteristics} \tab Implementation of the StatLog
project dataset characteristics
}

}
\seealso{\code{\link{characterize}}, \code{\link{datachar-visualization}}}
\references{See \emph{Eugster et al. (2010)} in \code{citation("benchmark")}.

R. D. King, C. Feng and A. Sutherland. STATLOG: Comparison of
classification algorithms on large real-world problems. Applied
Artifical Intelligence, 9, 1995.}
\alias{StatlogCharacteristics}

