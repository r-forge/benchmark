
subset.AlgorithmPerformance <- function(x, datasets = NULL,
                                        algorithms = NULL,
                                        performances = NULL) {

  if ( is.null(datasets) )
    datasets <- levels(x$datasets)

  if ( is.null(algorithms) )
    algorithms <- levels(x$algorithms)

  if ( is.null(performances) )
    performances <- levels(x$performances)
  
  
  idx <- x$datasets %in% datasets &
         x$algorithms %in% algorithms &
         x$performances %in% performances

  
  x[idx, ]
}

