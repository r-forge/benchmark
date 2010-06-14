

#' @param x An \code{\link{AlgorithmPerformance}} object
#' @param order.by Function like \code{\link{mean}}, \code{\link{median}},
#'   or \code{\link{max}} to calculate a display order of the algorithms;
#'   or \code{NULL} for no specific order.
#' @param dependence.show Show dependence of observations for all, none or
#'   outlier observations.
#' @param dependence.col Color of the dependence line.
#' @param ... Ignored.
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @method boxplot AlgorithmPerformance
#' @rdname algperf-visualization
#' @importFrom graphics boxplot
#' @S3method boxplot AlgorithmPerformance
boxplot.AlgorithmPerformance <- function(x, order.by = median,
                                         dependence.show = c("outliers", "all", "none"),
                                         dependence.col = alpha("black", 0.1), ...) {

  ## Make codetools (R CMD check) happy:
  algorithms <- value <- performances <- datasets <- samples <- NULL


  dependence.show <- match.arg(dependence.show)

  x <- order.algorithms.by(x, order.by)

  p <- ggplot(x, aes(algorithms, value))
  p <- p + facet_grid(performances ~ datasets, scales = "free")

  if ( dependence.show == "all" )
    p <- p + geom_line(aes(group = samples), colour = dependence.col)

  if ( dependence.show == "outliers" ) {
    # HACK: currently I don't know how to do that ggplot2-like.
    o <- unique(unlist(lapply(split(x$value, x$algorithms), which.outlier)))
    ox <- x[x$samples %in% o, ]

    p <- p + geom_line(aes(group = samples), data = ox, colour = dependence.col)
  }

  p <- p + geom_boxplot(aes(fill = algorithms)) +
    scale_fill_manual(values = attr(x, "algorithm_colors"))

  p
}



#' @param x An object
#' @param ... Additional arguments
#' @rdname benchmark-generics
#' @export
densityplot <- function(x, ...) {
  UseMethod("densityplot")
}



#' @param x An \code{\link{AlgorithmPerformance}} object
#' @param ... Ignored.
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @method densityplot AlgorithmPerformance
#' @rdname algperf-visualization
#' @S3method densityplot AlgorithmPerformance
densityplot.AlgorithmPerformance <- function(x, ...) {
  ## Make codetools (R CMD check) happy:
  algorithms <- value <- performances <- datasets <- samples <- NULL


  p <- ggplot(x, aes(x = value, colour = algorithms, group = algorithms))
  p <- p + facet_grid(performances ~ datasets, scales = "free")
  p <- p + geom_density(fill = NA) +
    scale_colour_manual(values = attr(x, "algorithm_colors"))

  p
}



#' @param x An \code{\link{AlgorithmPerformance}} object
#' @param order.by Function like \code{\link{mean}}, \code{\link{median}},
#'   or \code{\link{max}} to calculate a display order of the algorithms;
#'   or \code{NULL} for no specific order.
#' @param dependence.show Show dependence of observations for all or none
#'   observations.
#' @param dependence.col Color of the dependence line.
#' @param ... Ignored.
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @method stripchart AlgorithmPerformance
#' @rdname algperf-visualization
#' @importFrom graphics stripchart
#' @S3method stripchart AlgorithmPerformance
stripchart.AlgorithmPerformance <- function(x, order.by = median,
                                            dependence.show = c("none", "all"),
                                            dependence.col = alpha("black", 0.1), ...) {

  ## Make codetools (R CMD check) happy:
  algorithms <- value <- performances <- datasets <- samples <- NULL


  dependence.show <- match.arg(dependence.show)

  x <- order.algorithms.by(x, order.by)

  p <- ggplot(x, aes(x = algorithms, y = value, colour = algorithms))
  p <- p + facet_grid(performances ~ datasets, scales = "free")

  if ( dependence.show == "all" )
    p <- p + geom_line(aes(group = samples), colour = dependence.col)

  p <- p + geom_point() +
    scale_colour_manual(values = attr(x, "algorithm_colors"))

  p
}



### Internal functions: ##############################################

order.algorithms.by <- function(x, order.by) {
  if ( is.null(order.by) )
    return(x)

  order.by <- match.fun(order.by)

  x <- na.omit(x)

  o <- order(sapply(split(x$value, x$algorithms), order.by))
  l <- levels(x$algorithms)

  x$algorithms <- factor(x$algorithms,
                         ordered = TRUE,
                         levels = l[o])

  x
}



which.outlier <- function(x) {
  # Based on base:::boxplot.stats
  coef <- 1.5
  nna <- !is.na(x)
  n <- sum(nna)
  stats <- stats::fivenum(x, na.rm = TRUE)
  iqr <- diff(stats[c(2, 4)])
  out <- if (!is.na(iqr)) {
    x < (stats[2L] - coef * iqr) | x > (stats[4L] + coef *  iqr)
  }
  else {
    !is.finite(x)
  }

  which(out)
}


