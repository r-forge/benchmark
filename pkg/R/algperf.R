#' @include warehouse.R
{}



#' Return subsets of \code{AlgorithmPerformance} objects
#' @param x An \code{\link{AlgorithmPerformance}} object
#' @param datasets Selected datasets
#' @param algorithms Selected algorithms
#' @param performances Selected performances
#' @param samples Selected samples
#' @param ... Ignored
#' @return An \code{\link{AlgorithmPerformance}} object with just the
#'   selected observations
#' @method subset AlgorithmPerformance
#' @S3method subset AlgorithmPerformance
subset.AlgorithmPerformance <- function(x, datasets = NULL,
                                        algorithms = NULL,
                                        performances = NULL,
                                        samples = NULL, ...) {

  if ( is.null(datasets) )
    datasets <- levels(x$datasets)

  if ( is.null(algorithms) )
    algorithms <- levels(x$algorithms)

  if ( is.null(performances) )
    performances <- levels(x$performances)

  if ( is.null(samples) )
    samples <- levels(x$samples)


  idx <- x$datasets %in% datasets &
         x$algorithms %in% algorithms &
         x$performances %in% performances &
         x$samples %in% samples

  x <- x[idx, ]
  x$datasets <- x$datasets[, drop = TRUE]
  x$algorithms <- x$algorithms[, drop = TRUE]
  x$performances <- x$performances[, drop = TRUE]
  x$samples <- x$samples[, drop = TRUE]

  x
}

