

warehouse <- function(datasets, B,
                      algorithms = NULL,
                      performances = NULL,
                      characteristics = NULL,
                      tests = NULL) {

  if ( length(datasets) != length(B) )
    B <- rep(B, length(datasets))


  a <- mapply(DatasetList, datasets, B,
              MoreArgs = list(algorithms = algorithms,
                              performances = performances,
                              characteristics = characteristics,
                              tests = tests),
              SIMPLIFY = FALSE)
  names(a) <- datasets


  ## Proto object and default data views:
  a <- proto(data = a)

  a$meta <- list(datasets = datasets, B = B,
                 algorithms = algorithms,
                 performances = performances,
                 characteristics = characteristics,
                 tests = tests,
                 algorithm_colors = default_algorithm_colors(algorithms))


  if ( !is.null(algorithms) ) {
    setViewAlgorithmPerformance(a)
  }

  if ( !is.null(characteristics) ) {
    setViewDatasetCharacterization(a)
    setViewDatasetBasisCharacterization(a)
  }


  structure(a, class = c("warehouse", class(a)))
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
    view$samples <- as.factor(view$samples)

    view <- view[, c("samples", "datasets", "algorithms", "performances", "value")]


    structure(view, class = c("AlgorithmPerformance", class(view)),
              algorithm_colors = .$meta$algorithm_colors)
  }

  invisible(NULL)
}



setViewDatasetCharacterization <- function(object) {

  object$viewDatasetCharacterization <- function(.,
                                                 datasets = NULL,
                                                 characteristics = NULL) {

    if ( is.null(datasets) )
      datasets <- .$meta$datasets

    if ( is.null(characteristics) )
      characteristics <- .$meta$characteristics


    view <- lapply(.$data[datasets],
                   function(ds)
                   ds$DatasetCharacterization[,
                                              characteristics,
                                              drop = FALSE])
    attr(view, "varname") <- "datasets"

    view <- melt(view)
    view$datasets <- as.factor(view$datasets)
    view$samples <- as.factor(view$samples)

    view <- view[, c("samples", "datasets", "characteristics", "value")]

    basis <- .$viewDatasetBasisCharacterization(datasets = datasets,
                                                characteristics = characteristics)

    structure(view, class = c("DatasetCharacterization", class(view)),
              basis = basis)
  }

  invisible(NULL)
}



setViewDatasetBasisCharacterization <- function(object) {

  object$viewDatasetBasisCharacterization <- function(.,
                                                      datasets = NULL,
                                                      characteristics = NULL) {

    if ( is.null(datasets) )
      datasets <- .$meta$datasets

    if ( is.null(characteristics) )
      characteristics <- .$meta$characteristics


    view <- lapply(.$data[datasets],
                   function(ds)
                   ds$DatasetBasisCharacterization[,
                                                   characteristics,
                                                   drop = FALSE])
    attr(view, "varname") <- "datasets"

    view <- melt(view)
    view$datasets <- as.factor(view$datasets)

    view <- view[, c("datasets", "characteristics", "value")]


    structure(view, class = c("DatasetBasisCharacterization", class(view)))
  }

  invisible(NULL)
}



### Internal data structures: ########################################

WarehouseArray <- function(B, ..., class) {
  d <- list(...)

  dim <- c(B, sapply(d, length))
  dimnames <- c(list(samples = NULL), d)

  a <- array(NA_integer_, dim = dim, dimnames = dimnames)

  structure(a, class = c(class, class(a)))
}



AlgorithmPerformanceArray <- function(B, algorithms, performances) {
  WarehouseArray(B, algorithms = algorithms, performances = performances,
                 class = "AlgorithmPerformanceArray")
}



DatasetCharacterizationArray <- function(B, characteristics) {
  WarehouseArray(B, characteristics = characteristics,
                 class = "DatasetCharacterizationArray")
}



TestResultArray <- function(B, results) {
  WarehouseArray(B, results = results,
                 class = "TestResultArray")
}



DatasetList <- function(dataset, B,
                        algorithms = NULL,
                        performances = NULL,
                        characteristics = NULL,
                        tests = NULL) {

  a <- list()

  if ( !is.null(algorithms) & !is.null(performances) ) {
    a$AlgorithmPerformance <- AlgorithmPerformanceArray(B, algorithms,
                                                        performances)
  }

  if ( !is.null(characteristics) ) {
    a$DatasetCharacterization <- DatasetCharacterizationArray(B, characteristics)
    a$DatasetBasisCharacterization <- DatasetCharacterizationArray(1, characteristics)
  }

  if ( !is.null(tests) ) {
    a$TestResult <- TestResultArray(B, tests)
  }


  structure(a, class = c("DatasetList", class(a)),
            dataset = dataset)
}



### Internal functions: ##############################################

default_algorithm_colors <- function(algorithms) {
  # Based on ggplot2:::ScaleHue
  h <- c(0, 360) + 15
  l <- 65
  c <- 100

  start <- 1
  direction <- -1

  rotate <- function(x) (x + start) %% 360 * direction

  n <- length(algorithms)
  if ( (diff(h) %% 360) < 1 ) {
    h[2] <- h[2] - 360 / n
  }

  structure(grDevices::hcl(h = rotate(seq(h[1], h[2], length = n)),
                           c = c, l = l),
            names = algorithms)
}


