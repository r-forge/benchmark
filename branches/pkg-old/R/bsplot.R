
#' Benchmark experiment summary plot.
#'
#' The benchmark summary plot takes the individual benchmark
#' experiment results into account. The y-axis represents the
#' data sets, the x-axis a podium with as many places as
#' candidate algorithms.
#'
#' @param x The object to plot.
#' @param ... Unused
#' @export
bsplot <- function(x, ...) {
  UseMethod('bsplot')
}


#' @param x A \code{\link{becp}} object
#' @param stat A matrix with statistics to display (rows are
#'   the algorithms, columns the data sets)
#' @param ds.order Data set order
#' @method bsplot relation_ensemble
#' @S3method bsplot relation_ensemble
#' @rdname bsplot
bsplot.relation_ensemble <- function(x, stat = NULL, ds.order = NULL, alg.order = NULL, ...) {
  rm <- ranking(x)

  if ( !is.null(ds.order) ) {
    rm <- rm[,ds.order]
    stat <- stat[,ds.order]
  }
  if ( !is.null(alg.order) ) {
    rm <- rm[alg.order,]
    stat <- stat[alg.order,]
  }


  bsplot(rm, stat=stat, ...)
}


#' @param x A \code{\link{becp}} object
#' @param stat A matrix with statistics to display (rows are
#'   the algorithms, columns the data sets)
#' @param col Colors of the algorithms
#' @param xlab A title for the x axis
#' @param ylab A title for the y axis
#' @param sig.lwd Line width of the significance sperator line
#' @param stat.col Colors of the statistics
#' @method bsplot matrix
#' @S3method bsplot matrix
#' @rdname bsplot
bsplot.matrix <- function(x, stat = NULL,
                           col = structure(seq_len(nrow(x)) + 1,
                           names = rownames(x)),
                           ylab = 'Datasets', xlab = 'Podium', sig.lwd = 4,
                           stat.col = NULL, ylab.las = NULL, ...) {

  griddim <- dim(x)
  nalgs <- griddim[1]
  nds <- griddim[2]

  rtable <- apply(x, 2, function(y)names(sort(y)))


  ### Grid:
  rmargin <- 0.1
  rwidth <- 1
  rheight <- 1 - 2 * rmargin

  xleft <- (seq_len(nalgs)-1) * rwidth
  xright <- seq_len(nalgs) * rwidth
  ybottom <- rep(rmargin, nalgs)
  ytop <- rep((1-rmargin), nalgs)

  gxleft <- rep(xleft, nds)
  gxright <- rep(xright, nds)
  gybottom <- rep(ybottom, nds) + rep(seq_len(nds)-1, each=nalgs)
  gytop <- rep(ytop, nds) + rep(seq_len(nds)-1, each=nalgs)


  ### Significant lines:
  sx <- apply(x, 2, sort)
  nosig <- matrix(FALSE, nrow=nalgs, ncol=nds)

  for ( i in 1:(nalgs-1) )
    nosig[i,] <- sx[i,] == sx[i+1,]

  nosig[nalgs,] <- TRUE
  nosig <- as.vector(nosig)

  lx <- gxright[!nosig]
  lytop <- gytop[!nosig]
  lybottom <- gybottom[!nosig]


  ### Statistic bars:
  if ( !is.null(stat) ) {
    s <- matrix(NA, nrow=nalgs, ncol=nds)

    for ( i in seq_len(nds) )
      s[,i] <- stat[rtable[,i],i]

    sxleft <- gxleft
    sxright <- rep(seq_len(nalgs)-1,nds) + as.vector((s / max(s) * rwidth))
    sybottom <- gybottom + 0.1
    sytop <- gytop - 0.1
  }


  ### Plot:
  plot(1, type='n', xlim=c(0,nalgs), ylim=c(0,nds),
       axes=FALSE, xlab=xlab, ylab=ylab, ...)

  axis(1, labels=FALSE)
  mtext(paste(seq_len(nalgs), '.', sep=''),1,
        at=0.5+(seq_len(nalgs)-1), line=1)
  axis(2, at=0.5+(seq_len(nds)-1), labels=colnames(x), las = ylab.las)
  box()

  rect(gxleft, gybottom, gxright, gytop,
       col=col[as.vector(rtable)], border=NA)

  if ( !is.null(stat) )
    rect(sxleft, sybottom, sxright, sytop,
         col=stat.col[as.vector(rtable)], border=NA)

  mapply(function(x, yb, yt) {
           lines(rep(x,2), c(yb,yt), lend='butt', lwd=sig.lwd)
         },
         lx, lybottom, lytop)


  invisible(NULL)
}




