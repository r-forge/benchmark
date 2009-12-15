

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

  structure(d, class = c('characterized.dataset', class(d)))
}


c.characterized.dataset <- function(...) {
  structure(rbind(...), class = c('characterized.dataset', class(d)))
}

