#' @include warehouse.R
{}



#' Methods to coerce objects to a benchmark experiment warehouse.
#'
#' Coerces a \code{bench.result} object from package \code{mlr} to a
#' \code{\link{warehouse}} object.
#'
#' @param x A \code{bench.result} object from package \code{mlr}
#' @param ... Ignored
#' @return A \code{\link{warehouse}} object
#' @export
#' @title as.warehouse
#' @aliases as.warehouse
#' @rdname as.warehouse
as.warehouse.mlr.bench.result <- function(x, ...) {
  perf <- x@perf

  datasets <- names(perf)
  algorithms <- dimnames(perf[[1]])[[2]]
  performances <- dimnames(perf[[1]])[[3]]
  B <- sapply(perf, function(x) nrow(x) - 1)

  w <- warehouse(datasets, B,
                 algorithms = algorithms,
                 performances = performances)

  for ( d in datasets )
    w$data[[d]]$AlgorithmPerformance[, , ] <-
      perf[[d]][-nrow(perf[[d]]), ,]

  w
}



#' Methods to coerce objects to a benchmark experiment warehouse.
#'
#' Coerces a four dimensional array (1st: sampling, 2nd: algorithms,
#' 3rd: performance measures, 4th: datasets) to a
#' \code{\link{warehouse}} object.
#'
#' @param x A four dimensional array
#' @param ... Ignored
#' @return A \code{\link{warehouse}} object
#' @export
#' @rdname as.warehouse
as.warehouse.array4dim <- function(x, ...) {
  stopifnot(length(dim(x)) == 4)

  B <- dim(x)[1]
  algorithms <- dimnames(x)[[2]]
  performances <- dimnames(x)[[3]]
  datasets <- dimnames(x)[[4]]

  w <- warehouse(datasets, B,
                 algorithms = algorithms,
                 performances = performances)

  for ( d in length(datasets) )
    w$data[[d]]$AlgorithmPerformance[, , ] <- x[, , , d]

  w
}

