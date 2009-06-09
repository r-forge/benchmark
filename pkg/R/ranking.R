
#' Ranking of 
#' @param x The object to calculate a ranking from
#' @return 
#' @export
ranking <- function(x, ...) {
  UseMethod('ranking')
}


#' @param x A \code{\link{totallessr}} object
#' @
ranking.totallessr <- function(x) {
  return(ranking(relation_scores(x, decreasing=FALSE)))
}

ranking.relation <- function(x) {
  return(ranking(relation_scores(x, decreasing=FALSE)))
}

ranking.relation_ensemble <- function(x) {
  algs <- unlist(relation_domain(x)[[1]])
  rm <- sapply(x,
               function(x) {
                 ranking(x)[algs]
               })
  return(rm) 
}

ranking.default <- function(x) {
  return(structure(sort(rank(x, ties.method='min')),
                   class='ranking'))
}



### Pretty printer:

sequence <- function(x, ...) {
  UseMethod('sequence')
}

sequence.ranking <- function(x) {
  cl <- split(names(x), x)
  
  return(paste(lapply(cl, paste, collapse=' - '),
               collapse=' < '))
}

