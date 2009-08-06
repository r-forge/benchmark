

#' A relation which is reflexive and symmetrix is called an "approx"
#' relation. If the relation is even transitive, then it is called
#' a "totalapproxr" relation.
#'
#' @title "Equivalence" relation for benchmark experiments.
#' @param x Object to cast to the approxr relation
#' @return A \code{\link{relation}} object
#' @aliases totalapproxr
#' @export
approxr <- function(x, ...) {
  UseMethod('approxr')
}


#' @param x A \code{\link{pmatrix}} consisting of p-values
#' @param alpha Significance level
#' @method approxr pmatrix
#' @S3method approxr pmatrix
#' @rdname approxr
approxr.pmatrix <- function(x, alpha) {
 
  sig <- x > alpha
  sig[is.na(sig)] <- 0

  m <- diag(nrow(x))
  m <- m + sig + t(sig)

  return(approxr(m))
}


#' @param x An \code{\link{ciarray}} consisting of confidence intervals
#' @param relevance Relevance level
#' @method approxr ciarray
#' @S3method approxr ciarray
#' @rdname approxr
approxr.ciarray <- function(x, relevance=0) {

  nsig <- (x[,,'lwr'] < 0 & x[,,'upr'] > 0)
  nrel <- nsig | (x[,,'lwr'] > -relevance & x[,,'upr'] < relevance)

  nrel[is.na(nrel)] <- 0

  m <- diag(nrow(x))
  m <- m + nrel + t(nrel)

  
  return(approxr(m))
}


#' @param x A \code{\link{ranking}} object
#' @method approxr ranking
#' @S3method approxr ranking
#' @rdname approxr
approxr.ranking <- function(x) {

  i <- diag(length(x))
  dimnames(i) <- list(names(x), names(x))

  for ( r in names(x) )
    i[r,x[r] == x] <- i[x[r] == x,r] <- 1

  return(approxr(i))
}


#' @param x An incidence matrix
#' @method approxr matrix
#' @S3method approxr matrix
#' @rdname approxr
approxr.matrix <- function(x) {
  r <- relation(incidence=x)

  cl <- c('approxr', class(r))
  if ( relation_is_equivalence(r) )
    cl <- c('totalapproxr', cl)
  else
    warning('Relation is not a total approxr.')

  
  return(structure(r, class=cl))
}


print.approxr <- function(x) {
  cat('An approxr relation of size ', paste(dim(x), collapse=' x '), '.\n', sep='')
}


print.totalapproxr <- function(x) {
  cat('A total approxr relation of size ', paste(dim(x), collapse=' x '), '.\n', sep='')
  cat('Equivalence classes:\n')
  print(relation_classes(x))
}



#' A relation which is irreflexive and asymmetrix is called a "lessr"
#' relation. If the relation is even transitive and negative
#' transitive, it is called a "totallessr" relation.
#'
#' @title "Order" relation for benchmark experiments.
#' @param x Object to cast to the lessr relation
#' @return A \code{\link{relation}} object
#' @aliases totallessr
#' @seealso \code{\link{ranking}}
#' @export
lessr <- function(x, ...) {
  UseMethod('lessr')
}



#' @param x A \code{\link{pmatrix}} consisting of p-values
#' @param y A \code{\link{smatrix}} consisting of test statistics
#' @param alpha Significance level
#' @method lessr pmatrix
#' @S3method lessr pmatrix
#' @rdname lessr
lessr.pmatrix <- function(x, y, alpha) {

  sig <- x < alpha
  dir1 <- y > 0
  dir2 <- y < 0

  sig[is.na(sig)] <- 0
  dir1[is.na(dir1)] <- 0
  dir2[is.na(dir2)] <- 0

  m1 <- sig * dir1
  m2 <- t(sig) * t(dir2)

  
  return(lessr(m1 + m2))
}


#' @param x An \code{\link{ciarray}} consisting of confidence intervals
#' @param relevance Relevance level
#' @method lessr ciarray
#' @S3method lessr ciarray
#' @rdname lessr
lessr.ciarray <- function(x, relevance=0) {

  sig <- !(x[,,'lwr'] < 0 & x[,,'upr'] > 0)
  rel <- sig & !(x[,,'lwr'] > -relevance & x[,,'upr'] < relevance)

  mean <- (x[,,'lwr'] + x[,,'upr']) / 2
  dir1 <- mean > 0
  dir2 <- mean < 0

  rel[is.na(rel)] <- 0
  dir1[is.na(dir1)] <- 0
  dir2[is.na(dir2)] <- 0

  m1 <- rel * dir1
  m2 <- t(rel) * t(dir2)

  
  return(lessr(m1 + m2))
}


#' @param x A \code{\link{ranking}} object
#' @method lessr ranking
#' @S3method lessr ranking
#' @rdname lessr
lessr.ranking <- function(x) {

  i <- matrix(0, nrow=length(x), ncol=length(x),
              dimnames=list(names(x), names(x)))

  for ( r in names(x) )
    i[r, x[r] < x] <- 1

  return(lessr(i))
}


#' @param x An incidence matrix
#' @method lessr matrix
#' @S3method lessr matrix
#' @rdname lessr
lessr.matrix <- function(x) {
  r <- relation(incidence=x)

  cl <- c('lessr', class(r))
  if ( relation_is_strict_weak_order(r) )
    cl <- c('totallessr', cl)
  else
    warning('Relation is not a total lessr.')
  
  
  return(structure(r, class=cl))
}


print.lessr <- function(x) {
  cat('A lessr relation of size ', paste(dim(x), collapse=' x '), '.\n', sep='')
}


print.totallessr <- function(x) {
  cat('A total lessr relation of size ', paste(dim(x), collapse=' x '), '.\n', sep='')
  cat('Equivalence classes:\n')
  print(relation_classes(x))
  cat('Ranking:\n')
  print(ranking(x))
}
