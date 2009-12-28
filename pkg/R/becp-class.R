
#' Benchmark experiment container for performance.
#'
#' The \code{becp} \code{S3} class is a four dimensional array with the
#' dimensions \eqn{samples \times algorithms \times performances \times
#' datasets}{samples x algorithms x performances x datasets}.
#'
#' \code{\link{as.becp}} is the constructor function to create an object
#' of this class.
#'
#' @name becp-class
#' @aliases becp
#' @seealso \code{\link{as.becp}}
{}


#' @param x An object of class \code{\link{becp}}
#' @param ... Ignored
#' @param drop Coerced to the lowest possible dimension
#' @return The subset, either as \code{\link{becp}} object (\code{drop=FALSE})
#'   or as lowest possible object (\code{drop=TRUE})
#' @method [ becp
#' @S3method "[" becp
#' @rdname becp-class
`[.becp` <- function(x, ..., drop = FALSE) {
  y <- unclass(x)[..., drop = drop]

  if ( !drop )
    class(y) <- 'becp'

  return(y)
}


#' @param x An object of class \code{\link{becp}}
#' @param ... Ignored
#' @method print becp
#' @S3method print becp
#' @rdname becp-class
print.becp <- function(x, ...) {

  d <- dim(x)
  names(d) <- c('data sets', 'samples', 'algorithms', 'performances')

  cat('Algorithm performance container\n\n')
  print(d)
}


#' @param object An object of class \code{\link{becp}}
#' @param ... Ignored
#' @method summary becp
#' @S3method summary becp
#' @rdname becp-class
summary.becp <- function(object, ...) {

  d <- dim(object)
  dn <- dimnames(object)

  cat('Algorithm performance container\n\n')

  cat('Data sets:\n')
  a <- datasets(object)
  names(a) <- seq_along(a)
  print(a)

  cat('Number of samples:\n')
  print(d[1])
  cat('\n')


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
}


#' @param x An object of class \code{\link{becp}}
#' @return The names of the algorithms
#' @rdname becp-class
#' @export
algorithms <- function(x) {
  return(dimnames(x)$alg)
}


#' @param x An object of class \code{\link{becp}}
#' @return The names of the performance measures
#' @rdname becp-class
#' @export
performances <- function(x) {
  return(dimnames(x)$perf)
}


#' @param x An object of class \code{\link{becp}}
#' @return The names of the data sets
#' @rdname becp-class
#' @export
datasets <- function(x) {
  return(dimnames(x)$ds)
}


#' @param x An object of class \code{\link{becp}}
#' @return The number of samples
#' @rdname becp-class
#' @export
nsamples <- function(x) {
  return(dim(x)[1])
}



###
### Reshape methods:
###


#' Melt the bench object into the long format.
#'
#' @param data An object of class \code{\link{becp}}
#' @param na.rm Indicating whether \code{NA} values should be stripped
#' @param ... Ignored
#' @method melt becp
#' @S3method melt becp
#' @seealso \code{\link[reshape]{melt.array}}, \code{\link[reshape]{melt}}
melt.becp <- function(data, na.rm = TRUE, ...) {

  if ( na.rm ) {
    nas <- unique(which(is.na(data), arr.ind = TRUE)[,1])

    if ( length(nas) > 0 )
      data <- data[-nas, , , ]
  }

  df <- melt.array(unclass(data))
  df$samp <- factor(df$samp)
  df$alg <- factor(df$alg, levels = dimnames(data)$alg)

  #if ( na.rm ) {
  #  df <- na.omit(df)
  #  df$samp <- df$samp[,drop=TRUE]
  #}

  return(df)
}



###
### Cast methods:
###


#' Object constructor.
#'
#' @param x The object to cast
#' @param ... Ignored
#' @return The \code{\link{becp}} object
#' @export
as.becp <- function(x, ...) {
  UseMethod('as.becp')
}


#' as.becp.matrix
#'
#' Converts a matrix with samples as rows and algorithms
#' as columns, i.e., a benchmark experiment on one data set,
#' into a \code{\link{bencp}} object.
#'
#' @param x A matrix
#' @param perf The name of the performance measure
#' @param ds The name of the data set
#' @method as.becp matrix
#' @S3method as.becp matrix
#' @rdname as.becp
as.becp.matrix <- function(x, perf = '', ds = '', ...) {
  y <- array(dim = c(1, dim(x), 1, 1),
             dimnames=list(ds = ds,
                           samp = NULL,
                           alg = colnames(x),
                           perf = perf))
  y[1, , 1, 1] <- x


  return(structure(y, class = 'becp'))
}


#' as.becp.arry
#'
#' Converts an array, already in the correct format,
#' into a \code{\link{becp}} object.
#'
#' @param x An array (already in correct format)
#' @method as.becp array
#' @S3method as.becp array
#' @rdname as.becp
as.becp.array <- function(x, ...) {
  # TODO: verify x

  dimnames(x) <- list(ds = dimnames(x)[[1]],
                      samp = NULL,
                      alg = dimnames(x)[[3]],
                      perf = dimnames(x)[[4]])

  return(structure(x, class='bench'))
}


#' as.becp.list
#'
#' Converts a list of matrices (with equal dimensions)
#' of benchmark experiments on different data sets into
#' a \code{\link{becp}} object.
#'
#' @param x A list of matrices with equal dimensions
#' @param perf The name of the performance measure
#' @method as.bench list
#' @S3method as.bench list
#' @rdname as.bench
as.becp.list <- function(x, perf = '', ...) {
  # HACK: until now only casts list of matrices
  # with equal number of rows.

  y <- array(dim = c(length(x), nrow(x[[1]]), ncol(x[[1]]), 1),
             dimnames = list(ds = names(x),
                             samp = NULL,
                             alg = colnames(x[[1]]),
                             perf = perf))

  for ( i in seq(along = x) )
    y[, , , i] <- x[[i]]


  return(structure(y, class = 'becp'))
}


as.becp.NULL <- function(x = NULL, ds, B, algs, perf, ...) {
  a <- array(dim = c(length(ds), B, length(algs), length(perf)),
             dimnames = list(ds = ds,
                             samp = NULL,
                             alg = algs,
                             perf = perf))

  structure(a, class = 'becp')
}



