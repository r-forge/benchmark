
stripchart.bench <- function(x, col=seq_along(x),
                             ylab='Candidates', xlab=dimnames(x)$perf[1], ...) {
  d <- melt(x[,,1,1])
  stripchart(value ~ alg, data=d,
             col=col, ylab=ylab, xlab=xlab,  ...)
}
 
stripplot.bench <- function(x, data=NULL,
                            xlab='Candidates', ylab='Performance', ...) {
  d <- melt(x)
  stripplot(value ~ alg | perf * ds, data=d,
            xlab=xlab, ylab=ylab, ...)
}
