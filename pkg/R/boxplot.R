
boxplot.bench <- function(x, col=seq_along(x),
                          xlab='Candidates', ylab=dimnames(x)$perf[1], ...) {
  d <- melt(x[,,1,1])
  boxplot(value ~ alg, data=d,
          border=col, ylab=ylab, xlab=xlab,  ...)
}
 
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

