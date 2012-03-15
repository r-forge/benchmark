\name{plot.DatasetCharacterization}
\alias{plot.DatasetCharacterization}
\title{Visualization of dataset characteristics}
\usage{
  \method{plot}{DatasetCharacterization} (x, y = NULL,
    lines = TRUE, points = TRUE, null.line = TRUE,
    null.line.col = gray(0.7), basis = TRUE,
    basis.col = NULL, ...)
}
\arguments{
  \item{x}{A \code{\link{DatasetCharacterization}} object}

  \item{y}{Ignored}

  \item{lines}{Draw observation dependency lines}

  \item{points}{Draw observation points}

  \item{null.line}{Draw null line}

  \item{null.line.col}{Null line color}

  \item{basis}{Draw basis characterization of the dataset}

  \item{basis.col}{Color of basis characterization}

  \item{...}{Ignored}
}
\value{
  A \code{\link[ggplot2]{ggplot}} object.
}
\description{
  Visualization of dataset characteristics
}
\seealso{
  Other dataset.characterization: \code{\link{as.dataset}},
  \code{\link{characterize}},
  \code{\link{StatlogCharacteristics}}
}

