
#' Benchmark experiment plot.
#'
#' The benchmark experiment plot visualizes each benchmark
#' experiment run. The x-axis is a podium with as many places
#' as algorithms. For each benchmark run, the algorithms are
#' sorted according to their performance values and a dot is
#' drawn on the corresponding place. To visualize the count of
#' an algorithm on a specific position, a bar plot is shown for
#' each of podium places.
#'
#' @param x The object to plot.
#' @export
beplot <- function(x, ...) {
  UseMethod('beplot')
}


#' @param x A \code{\link{bench}} object
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param lines.show Connect dots of same benchmark runs
#' @param lines.col Line color
#' @param lines.alpha Alpha value of the line color
#' @param lines.lwd Line width
#' @param dots.pch Dot symbol
#' @param dots.cex Dot symbol expansion
#' @param places.lty Type of separator line between podium places
#' @param places.col Color of separator line between podium places
#' @param legendfn Function which draws a legend
#' @method beplot bench
#' @S3method beplot bench
#' @rdname beplot
beplot.bench <- function(x, col=1:ncol(x),
                         xlab='Podium', ylab=dimnames(x)$perf[1],
                         lines.show=FALSE, lines.alpha=0.2, lines.lwd=1, lines.col=col,
                         dots.pch=19, dots.cex=1,
                         places.lty=2, places.col=1,
                         legendfn=function(algs, cols){
                           legend('topleft', algs, lwd=1, col=cols, bg='white')}) {

  eval(ylab)
  
  x <- na.omit(x[,,1,1,drop=TRUE])
  beplot(x, col=col, xlab=xlab, ylab=ylab,
         lines.show=lines.show, lines.alpha=lines.alpha, lines.lwd=lines.lwd,
         lines.col=lines.col,
         dots.pch=dots.pch, dots.cex=dots.cex,
         places.lty=places.lty, places.col=places.col, legendfn=legendfn)
}


#' @param x A \code{\link{bench}} object
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param lines.show Connect dots of same benchmark runs
#' @param lines.col Line color
#' @param lines.alpha Alpha value of the line color
#' @param lines.lwd Line width
#' @param dots.pch Dot symbol
#' @param dots.cex Dot symbol expansion
#' @param places.lty Type of separator line between podium places
#' @param places.col Color of separator line between podium places
#' @param legendfn Function which draws a legend
#' @method beplot matrix
#' @S3method beplot matrix
#' @rdname beplot
beplot.matrix <- function(x, col=1:ncol(x), xlab='', ylab='',
                          lines.show=FALSE, lines.alpha=0.2, lines.lwd=1, lines.col=col,
                          dots.pch=19, dots.cex=1,
                          places.lty=2, places.col=1,
                          legendfn=function(algs, cols){
                            legend('topleft', algs, lwd=1, col=cols, bg='white')}) {

  nalgs <- ncol(x)
  algs <- colnames(x)


  # Medals table (see table.bench): 
  ranks <- t(apply(x, 1, as.ranking, ties='random', sorted=FALSE))
  nranks <- apply(ranks, 2, function(y)table(factor(y, levels=1:nalgs)))

  # Simple rank based global algorithm order
  # (see as.ranking.medalstable):
  barranks <- rank(colSums(x * (nalgs:1)/nalgs), ties='random')
  barorder <- order(barranks)

  
  ### Plot:
  dotplotborders <- (0:nalgs) * nalgs

  dotplaces <- (1:nalgs) - 0.5
  names(dotplaces) <- names(barranks)[barorder]

  barcols <- col
  dotcols <- col
  linecols <- sapply(lines.col,
                     function(c) {
                       r <- col2rgb(c)
                       rgb(r[1], r[2], r[3],
                           alpha=round(255*lines.alpha),
                           maxColorValue=255)
                     })  

  
  ## Draw it:
  opar <- par(no.readonly = TRUE)
  layout(matrix(c(1,2), nrow=2, byrow=TRUE), height=c(1,0.4))
  mar <- par('mar')
  
  # Figure 1: 
  par(mar=c(0, mar[2], mar[3], mar[4]))
  plot(dotplotborders, rep(max(x), nalgs+1),
       type='n', ylim=range(x), ylab=ylab, xlab='', axes=F)
  axis(1, at=dotplotborders, labels=NA, lwd=par('lwd'))
  axis(2, lwd=par('lwd'))
  box()

  # Podium place borders:
  abline(v=dotplotborders,
         lty=places.lty, col=places.col)

  # Content:
  linesegments <- function(x, y, ...) {
    n <- length(x)
    segments(x[-n], y[-n], x[-1], y[-1], ...)
  }
  
  drawthe <- function(fn, col, ...) {
    for ( i in 1:nrow(x) ) {
      r <- ranks[i,]
      o <- order(r)
    
      performances <- (x[i,])[o]
      places <- (dotplaces[names(r)] + ((r - 1) * nalgs))[o]

      fn(places, performances, col=col[o], ...)
    }
  }

  if ( lines.show )
    drawthe(linesegments, linecols, lwd=lines.lwd)
  
  drawthe(points, dotcols,
          pch=dots.pch, cex=dots.cex)

  legendfn(names(barranks)[barorder], dotcols[barorder])

  
  # Figure 2:
  par(mar=c(mar[1], mar[2], 0, mar[4]))
  barplot(t(nranks[,barorder]), beside=TRUE, width=1,
          axes=F, space=c(0,0), border=NA, ylim=c(0, nrow(x)),
          names.arg=paste(1:nalgs, '.', sep=''),
          col=col[barorder], xlab=xlab)
  axis(1, at=c(0, dotplotborders), labels=NA, lwd=par('lwd'))
  box()

  par(opar)
}
