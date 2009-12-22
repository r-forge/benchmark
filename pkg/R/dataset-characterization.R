

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
            class = c('mapped.dataset', 'list'),
            name = attr(y, 'name'))
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

  structure(x, class = c('reduced.dataset', class(x)),
            name = attr(y, 'name'))
}


characterize <- function(x, y, ...) {
  UseMethod('characterize')
}


characterize.dataset <- function(x, y, verbose = FALSE, index = NULL, ...) {
  stopifnot(is(y, 'characteristics'))

  d <- as.data.frame(reduce(map(x, y, verbose = verbose, index = index),
                            y, verbose = verbose))

  structure(d, class = c('characterization.frame', class(d)),
            name = attr(y, 'name'))
}


c.characterization.frame <- function(...) {
  structure(rbind(...), class = c('characterization.frame', 'data.frame'))
}


print.characterization.frame <- function(x, ...) {
  n <- nrow(x)
  m <- ncol(x)

  cat('Characterization frame:\n')
  cat(sprintf('%s dataset%s by %s characteristic%s\n',
              n, ifelse(n == 1, '', 's'),
              m, ifelse(m == 1, '', 's')))
}


scale.characterization.frame <- function(x) {
  rx <- apply(x, 2L, range, na.rm = TRUE)
  sx <- apply(x, 2L,
              function(x)
              (x - min(x, na.rm = TRUE)) /
              (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))

  sx[is.na(x)] <- -1

  m <- matrix(FALSE, nrow = nrow(x), ncol = ncol(x))
  m[, apply(rx, 2, function(x) length(unique(x)) == 1)] <- TRUE
  sx[m] <- 1

  sx
}


plot.characterization.frame <- function(x, y = NULL, ...) {
  data <- melt(scale(x))

  qplot(X2, value, data = data, group = X1, geom = c('point', 'line')) +
      facet_grid(X1 ~ .) +
      scale_y_continuous('') +
      scale_x_discrete(sprintf('%s Characteristics', attr(x, 'name'))) +
      theme_update(axis.text.x = theme_text(angle = 90, hjust = 1))
}



