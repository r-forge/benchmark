
densitychart <- function(x, ...) {
  UseMethod('densitychart')
}

densitychart.bench <- function(x, col=seq_along(x),
                               xlab=dimnames(x)$perf[1], ylab='Density',
                               dargs=list(), ...) {  
  d <- dim(x)
  l <- lapply(1:d[2], function(i) do.call('density', c(list(x[,i,,,drop=TRUE]),
                                                            dargs)))
  names(l) <- colnames(x)
  
  xra <- range(sapply(l, function(a)a$x))
  yra <- range(sapply(l, function(a)a$y))
  
  plot(1, 1, type='n', xlim=xra, ylim=yra,
       xlab=xlab, ylab=ylab)
  
  for ( i in seq_along(l) ) {
    lines(l[[i]], col=col[i], ...)
  }
}

densityplot.bench <- function(x, data=NULL,
                              xlab='Performance', ylab='Density',
                              plot.points=FALSE, auto.key=TRUE, ...) {

  d <- melt(x)
  densityplot(~ value | perf * ds, data=d,
              xlab=xlab, ylab=ylab, groups=alg,
              plot.points=plot.points, auto.key=auto.key, ...)
}
