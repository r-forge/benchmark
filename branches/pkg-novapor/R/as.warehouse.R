

as.warehouse.mlr.bench.result <- function(x, ...) {
  perf <- x@perf

  datasets <- names(perf)
  algorithms <- dimnames(perf[[1]])[[2]]
  performances <- dimnames(perf[[1]])[[3]]
  B <- sapply(perf, function(x) nrow(x) - 1)

  w <- warehouse(datasets, B,
                 algorithms = algorithms,
                 performances = performances)

  for ( d in datasets )
    w$data[[d]]$AlgorithmPerformance[, , ] <-
        perf[[d]][-nrow(perf[[d]]), ,]

  w
}


