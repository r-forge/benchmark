
#' Visualizations of algorithm performance.
#'
#' @name becp-plots
{}


#' @param x An object of class \code{\link{becp}}
#' @param col Color for each algorithm
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param ... Passed to underlying plot function
#' @return The value of the underlying \code{\link[graphics]{boxplot}} function
#' @method boxplot becp
#' @S3method boxplot becp
#' @rdname basicplots
boxplot.becp <- function(x, col = 1,
                         xlab = 'Candidates', ylab = dimnames(x)$perf[1], ...) {
  d <- melt(x[1, , , 1])
  boxplot(value ~ alg, data = d,
          border = col, ylab = ylab, xlab = xlab,  ...)
}


#' @param x An object of class \code{\link{becp}}
#' @param data Unused.
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param ... Passed to underlying plot function
#' @return The value of the underlying \code{\link[lattice]{bwplot}} function
#' @method bwplot becp
#' @S3method bwplot becp
#' @rdname basicplots
bwplot.becp <- function(x, data = NULL,
                        xlab = 'Candidates', ylab = 'Performance', ...) {

  d <- dim(x)
  f <- paste('value ~ alg | ',
             ifelse(d[3] > 1, 'perf *', ''),
             ifelse(d[4] > 1, 'ds', ''))

  d <- melt(x)
  bwplot(formula(f), data = d,
         xlab = xlab, ylab = ylab, ...)

  # TODO: scales?
}


#' @param x An object of class \code{\link{becp}}
#' @rdname basicplots
#' @export
densitychart <- function(x, ...) {
  UseMethod('densitychart')
}


#' @param x An object of class \code{\link{becp}}
#' @param col Color for each algorithm
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param dargs Arguments passed to the \code{\link[stats]{density}} function
#' @param ... Passed to underlying plot functions
#' @return Undefined
#' @method densitychart becp
#' @S3method densitychart becp
#' @rdname basicplots
densitychart.becp <- function(x, col = 1:ncol(x),
                              xlab = dimnames(x)$perf[1], ylab = 'Density',
                              dargs = list(), ...) {
  d <- dim(x)
  l <- lapply(1:d[2], function(i) do.call('density',
                                          c(list(x[, i, , , drop=TRUE]), dargs)))
  names(l) <- colnames(x)

  xra <- range(sapply(l, function(a)a$x))
  yra <- range(sapply(l, function(a)a$y))

  plot(1, 1, type = 'n', xlim = xra, ylim = yra,
       xlab = xlab, ylab = ylab, ...)

  for ( i in seq_along(l) ) {
    lines(l[[i]], col = col[i], ...)
  }
}


#' @param x An object of class \code{\link{becp}}
#' @param data Unused.
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param plot.points Specifying whether or not the data points
#'   should be plotted along with the estimated density
#' @param auto.key Key drawing, see \code{\link[lattice]{xyplot}}
#' @param ... Passed to underlying plot function
#' @return The value of the underlying \code{\link[lattice]{densityplot}} function
#' @method densityplot becp
#' @S3method densityplot becp
#' @rdname basicplots
densityplot.becp <- function(x, data = NULL,
                             xlab = 'Performance', ylab = 'Density',
                             plot.points = FALSE, auto.key = TRUE, ...) {

  d <- melt(x)
  densityplot(~ value | perf * ds, data = d,
              xlab = xlab, ylab = ylab, groups = 'alg',
              plot.points = plot.points, auto.key = auto.key, ...)
}


#' @param x An object of class \code{\link{becp}}
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
#' @method parallel becp
#' @S3method parallel becp
#' @rdname basicplots
parallel.becp <- function(x, data = NULL,
                          xlab = 'Candidates', ylab = dimnames(x)$perf[1],
                          horizontal.axis = FALSE, lines.col = 1,
                          common.scale = TRUE, ...) {

  d <- x[1, , , 1, drop = TRUE]

  parallel(d, xlab = xlab, ylab = ylab, horizontal.axis = horizontal.axis,
           col = lines.col, common.scale = common.scale, ...)
}


#' @param x An object of class \code{\link{becp}}
#' @param col Color for each algorithm
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param ... Passed to underlying plot function
#' @return The value of the underlying \code{\link[graphics]{stripchart}} function
#' @method stripchart becp
#' @S3method stripchart becp
#' @rdname basicplots
stripchart.becp <- function(x, col = 1,
                            ylab = 'Candidates', xlab = dimnames(x)$perf[1], ...) {
  d <- melt(x[1, , , 1])
  stripchart(value ~ alg, data = d,
             col = col, ylab = ylab, xlab = xlab, ...)
}


#' @param x An object of class \code{\link{becp}}
#' @param data Unused.
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param ... Passed to underlying plot function
#' @return The value of the underlying \code{\link[lattice]{stripplot}} function
#' @method stripplot becp
#' @S3method stripplot becp
#' @rdname basicplots
stripplot.becp <- function(x, data = NULL,
                            xlab = 'Candidates', ylab = 'Performance', ...) {
  d <- melt(x)
  stripplot(value ~ alg | perf * ds, data = d,
            xlab = xlab, ylab = ylab, ...)
}

