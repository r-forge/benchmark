#' @include warehouse.R
{}



#' Function to execute benchmark experiments and collect all data the
#' package can analyze. For more sophisticated benchmark experiments
#' we suggest the usage of the \code{mlr} package.
#'
#' @param datasets List of data.frames
#' @param sampling Sampling function, see \code{\link{benchmark-sampling}}.
#' @param algorithms List of algorithms; i.e., functions which take
#'   a model formula and a data.frame to fit a model. Note that a
#'   \code{\link[stats]{predict}} function must be defined as well.
#' @param performances List of performance measure functions; i.e.,
#'   functions with arguments \code{yhat} and \code{y}. See, e.g.,
#'   \code{\link{benchmark-comptime}}.
#' @param characteristics \code{\link{DatasetCharacteristics}} object
#' @param test \code{\link{Test}} object
#' @param verbose Show information during execution
#' @return A \code{\link{warehouse}} object
#' @seealso \code{\link{warehouse}}, \code{\link{as.warehouse}},
#'   \code{\link{benchmark-sampling}}, \code{\link{benchmark-comptime}}
#' @title Benchmark experiment execution
#' @export
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
  doTest <- FALSE

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

    if ( !is.null(test) ) {
      stopifnot(test$requirements())

      ntests <- c("pvalue", "statistic")
      doTest <- TRUE
    }
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
        }

        if ( doTest ) {
          printMsg(sprintf('    test\n'), verbose = verbose)

          accdat <- warehouse$viewAlgorithmPerformance(dataset = m)
          accdat <- na.omit(accdat)
          accdat$samples <- accdat$samples[, drop = TRUE]

          acctest <- test$new(accdat)$globalTest()
          warehouse$data[[m]]$TestResult[b, ] <- c(acctest$getPvalue(),
                                                   acctest$getStatistic())
        }
      }
    }

    printMsg('\n')
  }


  warehouse
}



### Sampling functions: ##############################################

#' Sampling functions.
#'
#' Functions to create a set of learning and test samples using a specific
#' resampling method.
#'
#' @param B Number of learning samples
#' @return List with bootstrap learning and test samples
#' @seealso \code{\link{benchmark}}
#' @rdname benchmark-sampling
#' @aliases benchmark-sampling
#' @export
bs.sampling <- function(B) {
  structure(B = B,
  function(n) {
    L <- lapply(1:B, function(.) sample(1:n, replace = TRUE))

    list(L = L,
         T = lapply(L, function(.) setdiff(1:n, .)))
  })
}



#' @param B Number of learning samples
#' @param psize Size of subsample
#' @return List with subsampling learning and test samples
#' @rdname benchmark-sampling
#' @export
sub.sampling <- function(B, psize) {
  structure(B = B, psize = psize,
  function(n) {
    size <- ceiling(n * psize)
    L <- lapply(1:B, function(.) sample(1:n, size, replace = FALSE))

    list(L = L,
         T = lapply(L, function(.) setdiff(1:n, .)))
  })
}



#' @param k Number of cross-validation samples
#' @return List with cross-validation learning and test samples
#' @rdname benchmark-sampling
#' @export
cv.sampling <- function(k) {
  structure(B = k,
  function(n) {
    T <- split(sample(1:n), rep(1:k, length = n))

    list(L = lapply(T, function(.) setdiff(1:n, .)),
         T = T)
  })
}



### Dummy time performance functions: ################################

#' Dummy functions to enable fitting and prediction time as performance
#' measures.
#'
#' @param yhat Ignored
#' @param y Ignored
#' @return Time (User and System) used for the model fitting
#' @seealso \code{\link{benchmark}}
#' @rdname benchmark-comptime
#' @aliases benchmark-comptime
#' @export
fittime <- function(yhat, y) {
  t <- get("ftime", envir = parent.frame())
  t[1] + t[2]
}



#' @param yhat Ignored
#' @param y Ignored
#' @return Time (User and System) used for the prediction
#' @rdname benchmark-comptime
#' @export
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

