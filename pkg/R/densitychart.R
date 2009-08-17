
#' @param x An object of class \code{\link{bench}}
#' @rdname basicplots
#' @export
densitychart <- function(x, ...) {
  UseMethod('densitychart')
}


#' @param x An object of class \code{\link{bench}}
#' @param col Color for each algorithm
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param dargs Arguments passed to the \code{\link[stats]{density}} function
#' @param ... Passed to underlying plot functions
#' @return Undefined
#' @method densitychart bench
#' @S3method densitychart bench
#' @rdname basicplots
densitychart.bench <- function(x, col=ncol(x),
                               xlab=dimnames(x)$perf[1], ylab='Density',
                               dargs=list(), ...) {  
  d <- dim(x)
  l <- lapply(1:d[2], function(i) do.call('density', c(list(x[,i,,,drop=TRUE]),
                                                            dargs)))
  names(l) <- colnames(x)
  
  xra <- range(sapply(l, function(a)a$x))
  yra <- range(sapply(l, function(a)a$y))
  
  plot(1, 1, type='n', xlim=xra, ylim=yra,
       xlab=xlab, ylab=ylab, ...)
  
  for ( i in seq_along(l) ) {
    lines(l[[i]], col=col[i], ...)
  }
}


#' @param x An object of class \code{\link{bench}}
#' @param data Unused.
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param plot.points Specifying whether or not the data points
#'   should be plotted along with the estimated density
#' @param auto.key Key drawing, see \code{\link[lattice]{xyplot}}
#' @param ... Passed to underlying plot function
#' @return The value of the underlying \code{\link[lattice]{densityplot}} function
#' @method densityplot bench
#' @S3method densityplot bench
#' @rdname basicplots
densityplot.bench <- function(x, data=NULL,
                              xlab='Performance', ylab='Density',
                              plot.points=FALSE, auto.key=TRUE, ...) {

  d <- melt(x)
  densityplot(~ value | perf * ds, data=d,
              xlab=xlab, ylab=ylab, groups='alg',
              plot.points=plot.points, auto.key=auto.key, ...)
}
