

benchmark <- function(formula, data, algorithms, performance, sampling,
                      data.characterize = TRUE, data.characteristics = statlog(),
                      verbose = TRUE, seed = NULL) {

  .msg <- function(x = '', newline = FALSE) {
    if ( verbose )
      cat(sprintf('%s%s', x, ifelse(newline, '\n', '')))
  }


  call <- match.call()

  .msg(deparse(call), newline = TRUE)

  if ( !is.null(seed) )
    set.seed(seed)


  data <- model.frame(formula, data = data)
  response <- attr(attributes(data)$terms, 'response')
  attr(data, 'terms') <- NULL

  n <- nrow(data)
  samples <- sampling(n)
  B <- length(samples$L)
  nalgs <- length(algorithms)

  becp <- as.becp(NULL,
                  ds = as.character(call$data),
                  B = B,
                  algs = as.character(call$algorithms)[-1],
                  perf = as.character(call$performance))

  if ( data.characterize ) {
    dataset <- as.dataset(formula, data)
    becc <- as.becc(characterize(dataset, data.characteristics), B)
  }

  for ( i in seq(length = B) ) {
    .msg('.')

    if ( data.characterize )
      becc[1, i, ] <- characterize(dataset, data.characteristics,
                                   index = samples$L[[i]])


    for ( j in seq(length = nalgs) ) {
      m <- algorithms[[j]](formula, data = data[samples$L[[i]], ])
      p <- predict(m, newdata = data[samples$T[[i]], -response])

      becp[1, i, j, ] <- performance(p, data[samples$T[[i]], response])
    }
  }


  ret <- structure(list(call = call,
                        becp = becp),
                   class = 'bec')

  if ( data.characterize )
    ret$becc <- becc


  return(ret)
}


print.bec <- function(x, full = TRUE, ...) {
  cat('Benchmark experiment container\n\n')
  cat(sprintf('%s%s', deparse(x$call), '\n'))
  cat('\n')

  if ( full )
    print(x[-1])
}



###
### Sampling schemes:
###

bs.sampling <- function(B) {
  function(n) {
    L <- lapply(1:B, function(.) sample(1:n, replace = TRUE))

    list(L = L,
         T = lapply(L, function(.) setdiff(1:n, .)))
  }
}


sub.sampling <- function(B, psize) {
  function(n) {
    size <- ceiling(n * psize)
    L <- lapply(1:B, function(.) sample(1:n, size, replace = FALSE))

    list(L = L,
         T = lapply(L, function(.) setdiff(1:n, .)))
  }
}


cv.sampling <- function(k) {
  function(n) {
    T <- split(sample(1:n), rep(1:k, length = n))

    list(L = lapply(T.index, function(.) setdiff(1:n, .)),
         T = T)
  }
}

