\name{benchmark-sampling}
\alias{bs.sampling}
\title{Sampling functions.}
\usage{bs.sampling(B)
sub.sampling(B, psize)
cv.sampling(k)
}
\description{Sampling functions.}
\details{\code{bs.sampling}: Functions to create a set of learning and test samples using a specific
resampling method.

}
\value{\code{bs.sampling}: List with bootstrap learning and test samples

\code{sub.sampling}: List with subsampling learning and test samples


\code{cv.sampling}: List with cross-validation learning and test samples


}
\seealso{\code{\link{benchmark}}}
\alias{benchmark-sampling}
\arguments{\item{B}{Number of learning samples}
\item{psize}{Size of subsample}
\item{k}{Number of cross-validation samples}
}
\alias{sub.sampling}
\alias{cv.sampling}

