
#' @param x An object of class \code{\link{bench}}
#' @param data Unused.
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param horizontal.axis Indicating whether the
#'   parallel axes should be laid out horizontally or vertically
#' @param lines.col Line colors
#' @param common.scale Whether a common scale should be used for
#'   the axis
#' @param ... Passed to underlying plot function
#' @return The value of the underlying \code{\link[lattice]{parallel}} function
#' @method parallel bench
#' @S3method parallel bench
#' @rdname basicplots
parallel.bench <- function(x, data=NULL,
                           xlab='Candidates', ylab=dimnames(x)$perf[1],
                           horizontal.axis=FALSE, lines.col=1,
                           common.scale=TRUE, ...) {

  d <- x[,,1,1,drop=TRUE]
  
  parallel(d, xlab=xlab, ylab=ylab, horizontal.axis=horizontal.axis,
           col=lines.col, common.scale=common.scale, ...)
}
