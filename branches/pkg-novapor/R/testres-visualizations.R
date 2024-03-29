

#' Visualization methods for (sequential) test results.
#' @param x An \code{\link{TestResult}} object
#' @param ... Ignored.
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @method plot TestResult
#' @rdname testres-visualization
#' @importFrom graphics plot
#' @S3method plot TestResult
plot.TestResult <- function(x, ...) {

  ## Make codetools (R CMD check) happy:
  value <- tests <- datasets <- samples <- NULL


  p <- ggplot(x, aes(samples, value))
  p <- p + facet_grid(tests ~ datasets, scale = "free_y")
  p <- p + geom_line()

  p
}
