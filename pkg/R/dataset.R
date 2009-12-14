

as.dataset <- function(formula, data, ordered.as.factor = TRUE) {

  ### Dataset base:
  ds <- new.env(parent = emptyenv())
  ds$.data <- data


  ### Dataset structure:
  details <- function(x, which) {
    classes <- sapply(data[, x, drop = FALSE],
                      function(var) {
                        if ( is.ordered(var) & ordered.as.factor )
                          return('factor')

                        class(var)
                    })


    list(list(structure(list(x), names = which)),
         lapply(split(classes, classes),
                function(x)
                list(list(structure(list(names(x)), names = which)),
                     structure(list(list(lapply(names(x),
                                                function(.)
                                                structure(list(.), names = which)))),
                               names = '.'))))
  }

  i2r.details <- function(x, y) {
    grid <- function(...)
      expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

    grid.details <- function(gx, gy) {
      varx <- x[[2]][[gx]][[c(1, 1, 1)]]
      vary <- y[[2]][[gy]][[c(1, 1, 1)]]

      var.grid <- grid(input = varx,
                       response = vary)
      var.grid <- apply(var.grid, 1, list)
      var.grid <- lapply(var.grid, '[[', 1)
      var.grid <- lapply(var.grid, as.list)

      list(list(list(input = varx, response = vary)),
           structure(list(list(var.grid)), names = '.'))
    }

    class.grid <- grid(names(x[[2]]), names(y[[2]]))

    list(list(list(input = x[[c(1, 1, 1)]],
                   response = y[[c(1, 1, 1)]])),
         structure(apply(class.grid, 1,
                         function(x)
                         grid.details(x[[1]], x[[2]])),
                   names = apply(class.grid, 1, paste, collapse = '2')))
  }

  formula <- terms(formula, data = data)
  variables <- as.character(as.list(attr(formula, 'variables'))[-1])
  response <- variables[attr(formula, 'response')]
  input  <- setdiff(variables, response)

  input.details <- details(input, 'input')
  response.details <- details(response, 'response')

  ds$.structure <- list(list(variables),
                        list(input = input.details,
                             response = response.details,
                             input2response = i2r.details(input.details,
                                                          response.details)))

  attributes(formula) <- NULL
  ds$.formula <- formula


  ### Dataset getter:
  ds$variables <- function(x = NULL) {
    m <- paste(sprintf('[[2]]$%s', x), collapse = '')
    e <- '[[1]]'
    g <- sprintf('ds$.structure%s%s', m, e)

    eval(parse(text = g))
  }

  ds$data <- function(x = NULL) {
    vars <- ds$variables(x)

    d <- lapply(vars,
                function(v)
                structure(lapply(v,
                                 function(.)
                                 ds$.data[, ., drop = ('.' %in% x)]),
                          names = names(v)))
    return(d)
  }


  ### Return object:
  structure(ds, class = 'dataset')
}


map <- function(x, y, ...) {
  UseMethod('map')
}


map.dataset <- function(x, y, verbose = TRUE, ...) {
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

                  d <- x$data(level)

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


characterize.dataset <- function(x, y, verbose = FALSE, ...) {
  stopifnot(is(y, 'characteristics'))

  as.data.frame(reduce(map(x, y, verbose = verbose), y, verbose = verbose))
}
