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
subset.AlgorithmPerformance <- function(x, subset,  ...) {
  subset <- substitute(subset)

  y <- subset.data.frame(x, eval(subset), ...)
  y$datasets <- y$datasets[, drop = TRUE]
  y$algorithms <- y$algorithms[, drop = TRUE]
  y$performances <- y$performances[, drop = TRUE]
  y$samples <- y$samples[, drop = TRUE]

  attr(y, "algorithm_colors") <- attr(x, "algorithm_colors")
  attr(y, "class") <- attr(x, "class")

  y
}

