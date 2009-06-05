
#' @param x An object of class \code{\link{bench}}
#' @param col Color for each algorithm
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param ... Passed to underlying plot function
#' @return The value of the underlying \code{\link[graphics]{boxplot}} function
#' @method boxplot bench
#' @rdname basicplots
boxplot.bench <- function(x, col=ncol(x),
                          xlab='Candidates', ylab=dimnames(x)$perf[1], ...) {
  d <- melt(x[,,1,1])
  boxplot(value ~ alg, data=d,
          border=col, ylab=ylab, xlab=xlab,  ...)
}


#' @param x An object of class \code{\link{bench}}
#' @param data Unused.
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param ... Passed to underlying plot function
#' @return The value of the underlying \code{\link[lattice]{bwplot}} function
#' @method bwplot bench
#' @rdname basicplots
bwplot.bench <- function(x, data=NULL,
                         xlab='Candidates', ylab='Performance', ...) {

  d <- dim(x)
  f <- paste('value ~ alg | ',
             ifelse(d[3] > 1, 'perf *', ''),
             ifelse(d[4] > 1, 'ds', ''))
  
  d <- melt(x)
  bwplot(formula(f), data=d,
         xlab=xlab, ylab=ylab, ...)

  # TODO: scales?
}

