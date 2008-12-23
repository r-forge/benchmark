
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
