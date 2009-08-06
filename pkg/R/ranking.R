
#' Ranking of relations.
#' @param x The relation to calculate a ranking
#' @return A \code{ranking} object
#' @export
ranking <- function(x, ...) {
  UseMethod('ranking')
}


#' @param x A \code{\link{totallessr}} object
#' @method ranking totallessr
#' @S3method ranking totallessr
#' @rdname ranking
ranking.totallessr <- function(x) {
  return(ranking(relation_scores(x, decreasing=FALSE)))
}


#' @param x A \code{\link{relation}} object
#' @method ranking relation
#' @S3method ranking relation
#' @rdname ranking
ranking.relation <- function(x) {
  return(ranking(relation_scores(x, decreasing=FALSE)))
}


#' @param x A \code{\link{relation_ensemble}} object
#' @method ranking relation_ensemble
#' @S3method ranking relation_ensemble
#' @rdname ranking
ranking.relation_ensemble <- function(x) {
  algs <- unlist(relation_domain(x)[[1]])
  rm <- sapply(x,
               function(x) {
                 ranking(x)[algs]
               })
  return(rm) 
}


#' @param x An object \code{rank} can work with
#' @method ranking default
#' @S3method ranking default
#' @rdname ranking
ranking.default <- function(x) {
  return(structure(sort(rank(x, ties.method='min')),
                   class='ranking'))
}



### Pretty printer:

#' @export
sequence <- function(x, ...) {
  UseMethod('sequence')
}


#' @param x A \code{\link{ranking}} object
#' @method sequence ranking
#' @S3method sequence ranking
#' @rdname ranking
#' @aliases sequence
sequence.ranking <- function(x) {
  cl <- split(names(x), x)
  
  return(paste(lapply(cl, paste, collapse=' - '),
               collapse=' < '))
}

