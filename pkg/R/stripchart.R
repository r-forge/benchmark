
#' @param x An object of class \code{\link{bench}}
#' @param col Color for each algorithm
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param ... Passed to underlying plot function
#' @return The value of the underlying \code{\link[graphics]{stripchart}} function
#' @method stripchart bench
#' @S3method stripchart bench
#' @rdname basicplots
stripchart.bench <- function(x, col=ncol(x),
                             ylab='Candidates', xlab=dimnames(x)$perf[1], ...) {
  d <- melt(x[,,1,1])
  stripchart(value ~ alg, data=d,
             col=col, ylab=ylab, xlab=xlab,  ...)
}


#' @param x An object of class \code{\link{bench}}
#' @param data Unused.
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param ... Passed to underlying plot function
#' @return The value of the underlying \code{\link[lattice]{stripplot}} function
#' @method stripplot bench
#' @S3method stripplot bench
#' @rdname basicplots
stripplot.bench <- function(x, data=NULL,
                            xlab='Candidates', ylab='Performance', ...) {
  d <- melt(x)
  stripplot(value ~ alg | perf * ds, data=d,
            xlab=xlab, ylab=ylab, ...)
}
