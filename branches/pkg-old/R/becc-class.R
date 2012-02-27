

#' Benchmark experiment container for dataset characterization.
#'
#' The \code{becc} \code{S3} class is a four dimensional array with the
#' dimensions \eqn{datasets \times samples \times characterization}
#' {datasets x samples x characterization}.
#'
#' \code{\link{as.becc}} is the constructor function to create an object
#' of this class.
#'
#' @name becc-class
#' @aliases becc
#' @seealso \code{\link{as.becc}}
{}


print.becc <- function(x, ...) {
  d <- dim(x)
  names(d) <- c('data sets', 'samples', 'characteristics')

  cat('Dataset characterization container\n\n')
  print(d)
}


plot.becc <- function(x, ds = 1, highlight = TRUE, highlight.col = 'red', 
                      highlight.alpha = 1, ...) {
  data <- x[ds, , ]
  data <- rbind(data, attr(x, 'base'))
  data <- structure(data,
                    class = c('characterization.matrix', 'matrix'),
                    dimnames = list(NULL, colnames(data)))

  p <- plot(data, colour = FALSE, ...)

  if ( highlight ) {
    msdata <- melt(scale(data))
    p <- p + geom_line(data = subset(msdata, subset = (X1 == nrow(data))),
                       colour = highlight.col, alpha = highlight.alpha)
    p <- p + geom_point(data = subset(msdata, subset = (X1 == nrow(data))),
                       colour = highlight.col, alpha = highlight.alpha)
  }

  p
}


###
### Cast methods:
###

as.becc <- function(x, ...) {
  UseMethod('as.becc')
}


as.becc.characterization.matrix <- function(x, samp, ...) {
  a <- array(dim = c(nrow(x), samp, ncol(x)),
             dimnames = list(ds = rownames(x),
                             samp = NULL,
                             chars = colnames(x)))

  structure(a, class = c('becc', class(a)), base = x)
}






