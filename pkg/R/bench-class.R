
`[.bench` <- function(x, ..., drop=FALSE) {
  y <- unclass(x)[...,drop=drop]

  if ( !drop )
    class(y) <- 'bench'

  return(y)
}

melt.bench <- function(data, na.rm=TRUE, ...) {

  df <- melt.array(unclass(data))
  df$samp <- factor(df$samp)
  
  if ( na.rm ) {
    df <- na.omit(df)
    df$samp <- df$samp[,drop=TRUE]
  }
  
  return(df)
}

print.bench <- function(object, ...) {

  d <- dim(object)
  names(d) <- c('samples', 'algorithms', 'performances', 'data sets')

  cat('Benchmark experiment\n\n')
  print(d)  
}

summary.bench <- function(object, ...) {

  d <- dim(object)
  dn <- dimnames(object)
  
  cat('Benchmark experiment\n\n')

  cat('Number of samples:', d[1], '\n\n')

  cat('Algorithms:\n')
  a <- algorithms(object)
  names(a) <- seq_along(a)
  print(a)
  cat('\n')
 
  cat('Performance measures:\n')
  a <- performances(object)
  names(a) <- seq_along(a)
  print(a)
  cat('\n')
  
  cat('Data sets:\n')
  a <- datasets(object)
  names(a) <- seq_along(a)
  print(a)
}

algorithms <- function(x) {
  return(dimnames(x)$alg)
}

performances <- function(x) {
  return(dimnames(x)$perf)
}

datasets <- function(x) {
  return(dimnames(x)$ds)
}

nsamples <- function(x) {
  return(dim(x)[1])
}


### Cast functions:

as.bench <- function(x, ...) {
  UseMethod('as.bench')
}

as.bench.matrix <- function(x, perf='', ds='', ...) {
  y <- array(dim=c(dim(x), 1, 1),
             dimnames=list(samp=NULL, alg=colnames(x),
               perf=perf, ds=ds))
  y[,,1,1] <- x

  
  return(structure(y, class='bench'))
}

as.bench.array <- function(x) {
  # TODO: verify x

  return(structure(x, class='bench'))
}

as.bench.list <- function(x, perf='', ...) {
  # HACK: until now only casts list of matrices
  # with equal number of rows.
  
  y <- array(dim=c(dim(x[[1]]), 1, length(x)),
             dimnames=list(samp=NULL, alg=colnames(x[[1]]),
               perf=perf, ds=names(x)))

  for ( i in 1:length(x) )
    y[,,,i] <- x[[i]]

  
  return(structure(y, class='bench'))
}
