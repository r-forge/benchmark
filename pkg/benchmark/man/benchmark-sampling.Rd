\name{bs.sampling}
\alias{benchmark-sampling}
\alias{bs.sampling}
\alias{cv.sampling}
\alias{sub.sampling}
\title{Sampling functions}
\usage{
  bs.sampling(B)

  sub.sampling(B, psize)

  cv.sampling(k)
}
\arguments{
  \item{B}{Number of learning samples}

  \item{psize}{Size of subsample}

  \item{k}{Number of cross-validation samples}
}
\value{
  List with corresponding learning and test samples
}
\description{
  Functions to create a set of learning and test samples
  using a specific resampling method.
}
\seealso{
  \code{\link{benchmark}}
}

