
#' Benchmark experiment base class.
#'
#' The \code{bench} \code{S3} class is a four dimensional array with the
#' dimensions \eqn{samples \times algorithms \times performances \times
#' datasets}{samples x algorithms x performances x datasets}.
#'
#' \code{\link{as.bench}} is the constructor function to create an object
#' of this class.
#'
#' @name bench-class
#' @aliases bench
#' @seealso \code{\link{as.bench}}
{}


#' @param x An object of class \code{\link{bench}}
#' @param ... Ignored
#' @param drop Coerced to the lowest possible dimension
#' @return The subset, either as \code{\link{bench}} object (\code{drop=FALSE})
#'   or as lowest possible object (\code{drop=TRUE})
#' @method [ bench
#' @S3method "[" bench
#' @rdname bench-class
`[.bench` <- function(x, ..., drop=FALSE) {
  y <- unclass(x)[...,drop=drop]

  if ( !drop )
    class(y) <- 'bench'

  return(y)
}


#' @param x An object of class \code{\link{bench}}
#' @param ... Ignored
#' @method print bench
#' @S3method print bench
#' @rdname bench-class
print.bench <- function(x, ...) {

  d <- dim(x)
  names(d) <- c('samples', 'algorithms', 'performances', 'data sets')

  cat('Benchmark experiment\n\n')
  print(d)
}


#' @param object An object of class \code{\link{bench}}
#' @param ... Ignored
#' @method summary bench
#' @S3method summary bench
#' @rdname bench-class
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


#' @param x An object of class \code{\link{bench}}
#' @return The names of the algorithms
#' @rdname bench-class
#' @export
algorithms <- function(x) {
  return(dimnames(x)$alg)
}


#' @param x An object of class \code{\link{bench}}
#' @return The names of the performance measures
#' @rdname bench-class
#' @export
performances <- function(x) {
  return(dimnames(x)$perf)
}


#' @param x An object of class \code{\link{bench}}
#' @return The names of the data sets
#' @rdname bench-class
#' @export
datasets <- function(x) {
  return(dimnames(x)$ds)
}


#' @param x An object of class \code{\link{bench}}
#' @return The number of samples
#' @rdname bench-class
#' @export
nsamples <- function(x) {
  return(dim(x)[1])
}



###
### Reshape functions:
###


#' Melt the bench object into the long format.
#'
#' @param data An object of class \code{\link{bench}}
#' @param na.rm Indicating whether \code{NA} values should be stripped
#' @param ... Ignored
#' @method melt bench
#' @S3method melt bench
#' @seealso \code{\link[reshape]{melt.array}}, \code{\link[reshape]{melt}}
melt.bench <- function(data, na.rm=TRUE, ...) {

  if ( na.rm ) {
    nas <- unique(which(is.na(data), arr.ind=TRUE)[,1])

    if ( length(nas) > 0 )
      data <- data[-nas,,,]
  }

  df <- melt.array(unclass(data))
  df$samp <- factor(df$samp)
  df$alg <- factor(df$alg, levels=dimnames(data)$alg)

  #if ( na.rm ) {
  #  df <- na.omit(df)
  #  df$samp <- df$samp[,drop=TRUE]
  #}

  return(df)
}



###
### Cast functions:
###


#' Object constructor.
#'
#' @param x The object to cast
#' @param ... Ignored
#' @return The \code{\link{bench}} object
#' @export
as.bench <- function(x, ...) {
  UseMethod('as.bench')
}


#' as.bench.matrix
#'
#' Converts a matrix with samples as rows and algorithms
#' as columns, i.e., a benchmark experiment on one data set,
#' into a \code{\link{bench}} object.
#'
#' @param x A matrix
#' @param perf The name of the performance measure
#' @param ds The name of the data set
#' @method as.bench matrix
#' @S3method as.bench matrix
#' @rdname as.bench
as.bench.matrix <- function(x, perf='', ds='', ...) {
  y <- array(dim=c(dim(x), 1, 1),
             dimnames=list(samp=NULL, alg=colnames(x),
               perf=perf, ds=ds))
  y[,,1,1] <- x


  return(structure(y, class='bench'))
}


#' as.bench.arry
#'
#' Converts an array, already in the correct format,
#' into a \code{\link{bench}} object.
#'
#' @param x An array (already in correct format)
#' @method as.bench array
#' @S3method as.bench array
#' @rdname as.bench
as.bench.array <- function(x, ...) {
  # TODO: verify x

  dimnames(x) <- list(samp=NULL, alg=dimnames(x)[[2]],
                      perf=dimnames(x)[[3]], ds=dimnames(x)[[4]])

  return(structure(x, class='bench'))
}


#' as.bench.list
#'
#' Converts a list of matrices (with equal dimensions)
#' of benchmark experiments on different data sets into
#' a \code{\link{bench}} object.
#'
#' @param x A list of matrices with equal dimensions
#' @param perf The name of the performance measure
#' @method as.bench list
#' @S3method as.bench list
#' @rdname as.bench
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
