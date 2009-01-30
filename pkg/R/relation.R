

approxr <- function(x, ...) {
  UseMethod('approxr')
}

approxr.pmatrix <- function(x, alpha) {
 
  sig <- x > alpha
  sig[is.na(sig)] <- 0

  m <- diag(nrow(x))
  m <- m + sig + t(sig)

  return(approxr(m))
}

approxr.ciarray <- function(x, relevance=0) {

  nsig <- (x[,,'lwr'] < 0 & x[,,'upr'] > 0)
  nrel <- nsig | (x[,,'lwr'] > -relevance & x[,,'upr'] < relevance)

  nrel[is.na(nrel)] <- 0

  m <- diag(nrow(x))
  m <- m + nrel + t(nrel)

  
  return(approxr(m))
}

approxr.default <- function(x) {
  r <- relation(incidence=x)

  cl <- c('approxr', class(r))
  if ( relation_is_equivalence(r) )
    cl <- c('totalapproxr', cl)
  else
    message('Relation is not a total approxr.')

  
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



lessr <- function(x, ...) {
  UseMethod('lessr')
}

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

lessr.default <- function(x) {
  r <- relation(incidence=x)

  cl <- c('lessr', class(r))
  if ( relation_is_strict_weak_order(r) )
    cl <- c('totallessr', cl)
  else
    message('Relation is not a total lessr.')
  
  
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






### Deprecated?

merge.relation_ensemble<- function(x, y=NULL, ...) {
  approx <- x$approx  
  less <- x$less
  # TODO: test for properties
  
  classes <- relation_classes(approx)
  representants <- sapply(classes, '[', 1)
  names <- lapply(classes, function(i)
                  paste(attr(classes, 'labels')[i], collapse=' - '))

  i <- relation_incidence(less)[representants,representants]
  dimnames(i) <- list(names, names)
  
  return(relation(incidence=i))
}



sequence <- function(r) {
  r <- transitive_reduction(r)
  i <- relation_incidence(r)
  # TODO: test for properties!

  names <- colnames(i)
  
  co <- which(colSums(i) == 0)
  s <- co
  for ( j in seq_len(length(names)-1) ) {
    co <- which(i[co,] == 1)
    s <- c(s, co)
  }

  return(names[unname(s)])
}






as.relation.tpairs <- function(x) {

  nalgs <- length(x$algs)

  # `relations`-bug workaround:
  x$algs <- sort(x$algs)
  
  incArray <- array(0, dim=c(nalgs, nalgs),
                    dimnames=list(x$algs, x$algs))

  for ( i in seq_along(x$pvalues) ) {  
    if ( x$pvalues[i] < x$alpha ) {
      as <- strsplit(names(x$pvalues)[i], ' - ', fixed=TRUE)[[1]]

      if ( x$tstat[i] < 0 )
        incArray[as[1],as[2]] <- 1
      else
        incArray[as[2],as[1]] <- 1
    }
  }

  return(as.relation(incArray))
}

as.relation.ranking <- function(x) {
  nalgs <- length(x)
  algs <- names(x)

  # `relations`-bug workaround:
  algs <- sort(algs)
  x <- x[algs]
  
  incArray <- array(0, dim=c(nalgs, nalgs),
                    dimnames=list(algs, algs))

  for ( i in 1:length(x) ) {
    gr <- (x[i] < x)
    incArray[algs[i], algs[gr]] <- 1
  }

  return(as.relation(incArray))
}

as.relation.relation_ensemble <- function(x) {
  incArray <- unclass(relation_incidence(x[[1]]))

  for ( i in 2:length(x) ) {
    a <- unclass(relation_incidence(x[[i]]))
    incArray <- incArray + a[rownames(incArray),colnames(incArray)]
  }

  incArray[incArray > 1] <- 1

  return(as.relation(incArray))
}

as.relation.confint <- function(x) {
  nalgs <- nrow(x)
  algs <- rownames(x)

  incArray <- array(0, dim=c(nalgs, nalgs),
                    dimnames=list(algs, algs))

  for ( i in 1:(nalgs-1) ) {
    for ( j in (i+1):nalgs ) {
      s <- max(x[i,1], x[j,1])
      e <- min(x[i,2], x[j,2])

      if ( e > s ) {
        incArray[algs[i],algs[j]] <- 1
        incArray[algs[j],algs[i]] <- 1
      }
    }
  }

  return(as.relation(incArray))
}


### Symmetric difference:

relation_symdiff_distance <- function(r1, r2) {
  if ( r1 == r2 ) 
    return(0)
  
  return(sum(relation_symdiff(r1, r2)$.Data$incidence))
}


relation_symdiff_matrix <- function(relations) {
  l <- length(relations)
  d <- matrix(NA, nrow=l, ncol=l)

  dimnames(d) <- list(names(relations), names(relations))

  for ( i in 1:(l-1) ) {
    for ( j in (i+1):l ) {
      d[i,j] <- relation_symdiff_distance(relations[[i]], relations[[j]])
      d[j,i] <- d[i,j]
    }
  }

  return(as.dist(d))
}
