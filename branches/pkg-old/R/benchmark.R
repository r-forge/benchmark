

benchmark <- function(data, sampling,
                      data.characterize = TRUE, data.characteristics = statlog(),
                      algorithms.test = TRUE, algorithms = NULL, performances = NULL,
                      verbose = TRUE, seed = NULL) {

  .msg <- function(x = '', newline = FALSE) {
    if ( verbose )
      cat(sprintf('%s%s', x, ifelse(newline, '\n', '')))
  }

  .args <- function(x) {
    y <- as.character(x)
    if ( length(y) > 1 )
      y <- y[-1]
    y
  }


  call <- match.call()
  .msg(deparse(call), newline = TRUE)

  if ( !is.null(seed) )
    set.seed(seed)


  if ( !is.list(data) )
    data <- list(data)

  if ( !is.list(algorithms) )
    algorithms <- list(algorithms)

  if ( !is.list(performances) )
    performances <- list(performances)

  B <- length(sampling(1)$L)


  ### Benchmark experiment containers:
  if ( algorithms.test )
    becp <- as.becp(NULL,
                    ds = .args(call$data),
                    B = B,
                    algs = .args(call$algorithms),
                    perf = .args(call$performances))

  if ( data.characterize )
    becc <- as.becc(do.call(c, lapply(data, characterize,
                                      data.characteristics)), B)


  ### The loop:
  for ( m in seq(along = data) ) {
    .msg(sprintf('m = %s\n', m))

    samples <- sampling(nrow(data[[m]]$data()))

    for ( b in seq(length = B) ) {
      .msg(sprintf('  b = %s\n', b))

      if ( data.characterize )
        becc[m, b, ] <- characterize(data[[m]], data.characteristics,
                                     index = samples$L[[b]])

      if ( algorithms.test ) {
        for ( k in seq(along = algorithms) ) {
          .msg(sprintf('    k = %s\n', k))

          fit <- algorithms[[k]](data[[m]]$formula(),
                                 data = data[[m]]$data(index = samples$L[[b]]))

          pred <- predict(fit,
                          newdata = data[[m]]$input(index = samples$T[[b]]))

          for ( p in seq(along = performances ) ) {
             .msg(sprintf('      p = %s\n', p))

              becp[m, b, k, p] <- performances[[p]](pred,
                                                    data[[m]]$response(index =
                                                                       samples$T[[b]])[[1]])
          }
        }
      }
    }

    .msg('\n')
  }


  ret <- structure(list(call = call), class = 'bec')

  if ( algorithms.test )
    ret$becp <- becp

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


