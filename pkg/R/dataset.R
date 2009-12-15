

as.dataset <- function(formula, data, ordered.as.factor = TRUE, integer.as.numeric = TRUE) {

  ### Dataset base:
  ds <- new.env(parent = emptyenv())
  ds$.data <- data


  ### Dataset structure:
  details <- function(x, which) {
    classes <- sapply(data[, x, drop = FALSE],
                      function(var) {
                        if ( is.ordered(var) & ordered.as.factor )
                          return('factor')

                        if ( is.integer(var) & integer.as.numeric )
                          return('numeric')

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

  ds$data <- function(x = NULL, index = NULL) {
    vars <- ds$variables(x)

    if ( is.null(index) )
      index <- seq(length = nrow(ds$.data))

    d <- lapply(vars,
                function(v)
                structure(lapply(v,
                                 function(.)
                                 ds$.data[index, ., drop = ('.' %in% x)]),
                          names = names(v)))
    return(d)
  }


  ### Return object:
  structure(ds, class = 'dataset')
}


print.dataset <- function(x, ...) {
  cat('Dataset object:\n')
  print(x$.formula)

  invisible(x)
}


