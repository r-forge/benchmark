

map <- function(x, y, ...) {
  UseMethod('map')
}


map.dataset <- function(x, y, verbose = TRUE, index = NULL, ...) {
  stopifnot(is(y, 'characteristics'))

  traverse.tree <- function(tree, level = NULL) {
    l <- lapply(names(tree),
                function(nodename) {
                  if ( is(tree[[nodename]], 'list') )
                    return(traverse.tree(tree[[nodename]],
                                         c(level, nodename)))

                  if ( verbose )
                    cat(sprintf('map: %s -> %s\n', paste(level, collapse = '.'),
                                                   nodename))

                  d <- x$data(level, index = index)

                  if ( length(d) == 0 )
                    return(NA)

                  sapply(d, function(x) do.call(tree[[nodename]], unname(x)))
              })

    structure(l, names = names(tree))
  }

  structure(traverse.tree(y$map),
            class = c('mapped.dataset', 'list'))
}


reduce <- function(x, y, ...) {
  UseMethod('reduce')
}


reduce.mapped.dataset <- function(x, y, verbose = TRUE, ...) {
  stopifnot(is(y, 'characteristics'))

  traverse.tree <- function(tree, level = NULL) {
    lapply(names(tree),
           function(nodename) {
             if ( is(tree[[nodename]], 'list') )
               return(traverse.tree(tree[[nodename]],
                                    c(level, nodename)))

             if ( verbose )
               cat(sprintf('reduce: %s\n', paste(c(level, nodename), collapse = '.')))

             f <- tree[[nodename]]

             if ( is.function(f) )
               x[[c(level, nodename)]] <<- f(x[[c(level, nodename)]])

             if ( is.null(f) )
               x[[c(level, nodename)]] <<- NULL

             if ( is(f, 'p') )
               x[[c(level, nodename)]] <<- do.call(f$fn,
                                                   lapply(f$args,
                                                          function(.) x[[.]]))
           })
  }

  traverse.tree(y$reduce)

  structure(x, class = c('reduced.dataset', class(x)))
}


characterize <- function(x, y, ...) {
  UseMethod('characterize')
}


characterize.dataset <- function(x, y, verbose = FALSE, index = NULL, ...) {
  stopifnot(is(y, 'characteristics'))

  d <- as.data.frame(reduce(map(x, y, verbose = verbose, index = index),
                            y, verbose = verbose))

  structure(d, class = c('characterization.data.frame', class(d)))
}


c.characterization.data.frame <- function(...) {
  structure(rbind(...), class = c('characterization.data.frame', 'data.frame'))
}


print.characterization.data.frame <- function(x, ...) {
  n <- nrow(x)
  m <- ncol(x)

  cat('Characterization data frame:\n')
  cat(sprintf('%s dataset%s by %s characteristic%s\n',
              n, ifelse(n == 1, '', 's'),
              m, ifelse(m == 1, '', 's')))
}


plot.characterization.data.frame <- function(x, y = NULL, lty = 1, col = 1,
                                             var.label = TRUE,  ...) {
  rx <- apply(x, 2L, range, na.rm = TRUE)
  x2 <- apply(x, 2L,
              function(x) (x - min(x, na.rm = TRUE))/(max(x,
                                      na.rm = TRUE) - min(x, na.rm = TRUE)))
  x2[is.na(x)] <- -1
  x2[

  matplot(1L:ncol(x), t(x), type = "l", col = col, lty = lty,
          xlab = "", ylab = "", axes = FALSE, ...)
  axis(1, at = 1L:ncol(x), labels = colnames(x))
  for (i in 1L:ncol(x)) {
    lines(c(i, i), c(0, 1), col = "grey70")
    if (var.label)
        text(c(i, i), c(0, 1), labels = format(rx[, i], digits = 3),
             xpd = NA, offset = 0.3, pos = c(1, 3), cex = 0.7)
  }

  invisible(x)
}
