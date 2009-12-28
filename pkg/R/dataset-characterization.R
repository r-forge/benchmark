

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

                  d <- x$dataparts(level, index = index)

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



###
### User interface:
###

characterize <- function(x, y, ...) {
  UseMethod('characterize')
}


characterize.dataset <- function(x, y, verbose = FALSE, index = NULL, ...) {
  stopifnot(is(y, 'characteristics'))

  d <- reduce(map(x, y, verbose = verbose, index = index),
              y, verbose = verbose)
  d <- as.matrix(as.data.frame(d))
  rownames(d) <- x$.dataname

  structure(d, class = c('characterization.matrix', class(d)),
            name = attr(y, 'name'))
}


print.characterization.matrix <- function(x, ...) {
  d <- dim(x)
  names(d) <- c('datasets', 'characteristics')

  cat('Dataset characterization matrix\n\n')
  print(d)
}


c.characterization.matrix <- function(...) {
  structure(rbind(...), class = c('characterization.matrix', 'matrix'))
}


scale.characterization.matrix <- function(x) {
  rx <- apply(x, 2L, range, na.rm = TRUE)
  sx <- apply(x, 2L,
              function(x)
              (x - min(x, na.rm = TRUE)) /
              (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))

  sx[is.na(x)] <- -0.2

  m <- matrix(FALSE, nrow = nrow(x), ncol = ncol(x))
  m[, apply(rx, 2, function(x) length(unique(x)) == 1)] <- TRUE
  sx[m] <- 1

  sx
}


plot.characterization.matrix <- function(x, y = NULL, facet = FALSE, colour = !facet,
                     null.line = TRUE, null.line.col = gray(0.7),
                     ...) {
  data <- melt(scale(x))
  data$X1 <- as.factor(data$X1)

  p <- {
    if ( colour )
      ggplot(data, aes(X2, value, group = X1, colour = X1))
    else
      ggplot(data, aes(X2, value, group = X1))
  }

  if ( null.line )
    p <- p + geom_hline(aes(yintercept = 0), colour = null.line.col)

  p <- p + geom_line()
  p <- p + geom_point()

  p <- p + scale_y_continuous('', breaks = c(-0.2, seq(0, 1, by = 0.2)),
                              labels = c('NA', seq(0, 1, by = 0.2))) +
           scale_x_discrete(sprintf('%s Characteristics', attr(x, 'name'))) +
           theme_update(axis.text.x = theme_text(angle = 90, hjust = 1))

  if ( facet )
    p <- p + facet_grid(X1 ~ .)

  if ( colour )
    p <- p + scale_colour_hue('Datasets')

  p
}

