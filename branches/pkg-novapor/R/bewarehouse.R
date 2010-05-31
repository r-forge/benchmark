

bewarehouse <- function(datasets, B,
                        algorithms = NULL,
                        performances = NULL,
                        characteristics = NULL,
                        results = NULL) {

  if ( length(datasets) != length(B) )
    B <- rep(B, length(datasets))


  a <- mapply(DatasetList, datasets, B,
              MoreArgs = list(algorithms = algorithms,
                              performances = performances,
                              characteristics = characteristics,
                              results = results),
              SIMPLIFY = FALSE)
  names(a) <- datasets


  ## Proto object and default data views:
  a <- proto(data = a)

  a$meta <- list(datasets = datasets, B = B,
                 algorithms = algorithms,
                 performances = performances,
                 characteristics = characteristics,
                 results = results)

  setViewAlgorithmPerformance(a)


  structure(a, class = c("bewarehouse", class(a)))
}



### Data views: ######################################################

setViewAlgorithmPerformance <- function(object) {

  object$viewAlgorithmPerformance <- function(.,
                                              datasets = NULL,
                                              algorithms = NULL,
                                              performances = NULL) {
    
    if ( is.null(datasets) )
      datasets <- .$meta$datasets

    if ( is.null(algorithms) )
      algorithms <- .$meta$algorithms

    if ( is.null(performances) )
      performances <- .$meta$performances
    

    view <- lapply(.$data[datasets],
                   function(ds)
                   ds$AlgorithmPerformance[,
                                           algorithms,
                                           performances,
                                           drop = FALSE])
    attr(view, "varname") <- "datasets"

    view <- melt(view)
    view$datasets <- as.factor(view$datasets)

    view <- view[, c("samples", "datasets", "algorithms", "performances", "value")]

    
    structure(view, class = c("AlgorithmPerformance", class(view)))
  }

  invisible()
}



### Internal data structures: ########################################

BeWarehouseArray <- function(B, ..., class) {
  d <- list(...)

  dim <- c(B, sapply(d, length))
  dimnames <- c(list(samples = NULL), d)

  a <- array(NA_integer_, dim = dim, dimnames = dimnames)

  structure(a, class = c(class, class(a)))
}



AlgorithmPerformanceArray <- function(B, algorithms, performances) {
  BeWarehouseArray(B, algorithms = algorithms, performances = performances,
                   class = "AlgorithmPerformanceArray")
}



DatasetCharacterizationArray <- function(B, characteristics) {
  BeWarehouseArray(B, characteristics = characteristics,
                   class = "DatasetCharacterizationArray")
}



TestResultArray <- function(B, results) {
  BeWarehouseArray(B, results = results,
                   class = "TestResultArray")
}



DatasetList <- function(dataset, B,
                        algorithms = NULL,
                        performances = NULL,
                        characteristics = NULL,
                        results = NULL) {

  a <- list()

  if ( !is.null(algorithms) & !is.null(performances) )
    a$AlgorithmPerformance <- AlgorithmPerformanceArray(B, algorithms, performances)

  if ( !is.null(characteristics) )
    a$DatasetCharacterization <- DatasetCharacterizationArray(B, characteristics)

  if ( !is.null(results) )
    a$TestResult <- TestResultArray(B, results)


  structure(a, class = c("DatasetList", class(a)),
            dataset = dataset)
}



