

benchmark <- function(formula, data, algorithms, performance, sampling,
                      data.characterize = TRUE, data.characteristics = statlog(),
                      verbose = TRUE) {

  .msg <- function(x = '', newline = FALSE) {
    if ( verbose )
      cat(sprintf('%s%s', x, ifelse(newline, '\n', '')))
  }


  call <- match.call()

  .msg(deparse(call), newline = TRUE)

  if ( data.characterize ) {
    dataset <- as.dataset(formula, data)
    dataset.ch <- characterize(dataset, data.characteristics)
  }

  data <- model.frame(formula, data = data)
  response <- attr(attributes(data)$terms, 'response')
  attr(data, 'terms') <- NULL

  n <- nrow(data)

  samples <- sampling(n)
  B <- length(samples$L)

  nalgs <- length(algorithms)
  bench <- matrix(NA, ncol = nalgs, nrow = B)

  for ( i in seq(length = B) ) {
    .msg(sprintf('%s: ', i))

    if ( data.characterize ) {
      .msg('-')

      dataset.ch <- c(dataset.ch,
                      characterize(dataset, data.characteristics,
                                   index = samples$L[[i]]))

    }

    for ( j in seq(length = nalgs) ) {
      .msg('.')

      m <- algorithms[[j]](formula, data = data[samples$L[[i]], ])
      p <- predict(m, newdata = data[samples$T[[i]], -response])

      bench[i, j] <- performance(p, data[samples$T[[i]], response])
    }

    .msg(newline = TRUE)
  }


  colnames(bench) <- as.character(call$algorithms)[-1]
  bench <- as.bench(bench, perf = as.character(call$performance),
                    ds = as.character(call$data))

  if ( data.characterize )
      return(structure(list(bench = bench,
                            dataset = dataset.ch[-1, ]),
                       class = 'psychobench'))

  return(bench)
}



### Sampling schemes:

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

