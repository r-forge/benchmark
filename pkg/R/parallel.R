

parallel.bench <- function(x, data=NULL,
                           xlab='Candidates', ylab=dimnames(x)$perf[1],
                           horizontal.axis=FALSE, lines.col=1,
                           common.scale=TRUE, ...) {

  d <- x[,,1,1,drop=TRUE]
  
  parallel(d, xlab=xlab, ylab=ylab, horizontal.axis=horizontal.axis,
           col=lines.col, common.scale=common.scale, ...)
}
