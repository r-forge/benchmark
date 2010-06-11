

benchmark <- function(datasets, sampling, algorithms = NULL,
                      performances = NULL, characteristics = NULL,
                      test = NULL, verbose = TRUE) {

  call <- match.call()


  ## Check what to do:
  if ( !is.list(datasets) )
    datasets <- list(datasets)

  B <- attr(sampling, "B")

  doAlgorithmPerformances <- FALSE
  doCharacterization <- FALSE
  doTests <- FALSE

  ndatasets <- deparseArgs(call$datasets)
  nalgorithms <- NULL
  nperformances <- NULL
  ncharacteristics <- NULL
  ntests <- NULL

  if ( !is.null(algorithms) && !is.null(performances) ) {
    if ( !is.list(algorithms) )
      algorithms <- list(algorithms)

    if ( !is.list(performances) )
      performances <- list(performances)

    nalgorithms <- deparseArgs(call$algorithms)
    nperformances <- deparseArgs(call$performances)

    doAlgorithmPerformances <- TRUE
  }
  else {
    stopifnot(!is.null(algorithms) && !is.null(performances))
  }

  if ( !is.null(characteristics) ) {
    ncharacteristics <- characteristics$characteristics()
    doCharacterization <- TRUE
  }


  ## Warehouse:
  warehouse <- warehouse(datasets = ndatasets, B = B,
                         algorithms = nalgorithms,
                         performances = nperformances,
                         characteristics = ncharacteristics,
                         tests = ntests)


  ## Loop:
  for ( m in seq(along = datasets) ) {
    printMsg(sprintf('m = %s\n', m), verbose = verbose)

    if ( doCharacterization )
      warehouse$data[[m]]$DatasetBasisCharacterization[, ] <-
            characterize(datasets[[m]], characteristics)


    samples <- sampling(nrow(datasets[[m]]$data()))


    for ( b in seq(length = B) ) {
      printMsg(sprintf('  b = %s\n', b), verbose = verbose)


      if ( doCharacterization )
        warehouse$data[[m]]$DatasetCharacterization[b, ] <-
            characterize(datasets[[m]],
                         characteristics,
                         index = samples$L[[b]])


      if ( doAlgorithmPerformances ) {
        for ( k in seq(along = algorithms) ) {
          printMsg(sprintf('    k = %s\n', k), verbose = verbose)

          ftime <- system.time(
            fit <- algorithms[[k]](datasets[[m]]$formula(),
                                   data = datasets[[m]]$data(index = samples$L[[b]])))

          ptime <- system.time(
            pred <- predict(fit,
                            newdata = datasets[[m]]$input(index = samples$T[[b]])))

          for ( p in seq(along = performances ) ) {
            printMsg(sprintf('      p = %s\n', p), verbose = verbose)

            warehouse$data[[m]]$AlgorithmPerformance[b, k, p] <-
                performances[[p]](pred,
                                  datasets[[m]]$response(index = samples$T[[b]])[[1]])
          }


          #if ( doTests ) {
          #  test <- paircomp(warehouse$AlgorithmPerformance(dataset = m,
          #                                                samples = 1:b),
          #                 family = tests)
          #  result <- pc$pairwiseTest()
          #  # ...
          #
          #}
        }
      }
    }

    printMsg('\n')
  }


  warehouse
}



### Sampling functions: ##############################################

bs.sampling <- function(B) {
  structure(B = B,
  function(n) {
    L <- lapply(1:B, function(.) sample(1:n, replace = TRUE))

    list(L = L,
         T = lapply(L, function(.) setdiff(1:n, .)))
  })
}



sub.sampling <- function(B, psize) {
  structure(B = B, psize = psize,
  function(n) {
    size <- ceiling(n * psize)
    L <- lapply(1:B, function(.) sample(1:n, size, replace = FALSE))

    list(L = L,
         T = lapply(L, function(.) setdiff(1:n, .)))
  })
}



cv.sampling <- function(k) {
  structure(B = k,
  function(n) {
    T <- split(sample(1:n), rep(1:k, length = n))

    list(L = lapply(T.index, function(.) setdiff(1:n, .)),
         T = T)
  })
}



### Dummy time performance functions: ################################

fittime <- function(yhat, y) {
  t <- get("ftime", envir = parent.frame())
  t[1] + t[2]
}

predicttime <- function(yhat, y) {
  t <- get("ptime", envir = parent.frame())
  t[1] + t[2]
}



### Internal functions: ##############################################

printMsg <- function(x = "", newline = FALSE, verbose = TRUE) {
  if ( verbose )
    cat(sprintf("%s%s", x, ifelse(newline, "\n", "")))
}



deparseArgs <- function(x) {
  y <- as.character(x)
  if ( length(y) > 1 )
    y <- y[-1]

  y
}

