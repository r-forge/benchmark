
tsort <- function(x, ...) {
  UseMethod('tsort')
}

tsort.relation <- function(x, ...) {
  
  ancestors <- colSums(relation_incidence(x))
  n <- length(ancestors)
  
  order <- character(length=n)
  attr(order, 'porder') <- character(length=n)
  
  v <- -Inf
  for ( i in 1:n ) {
    m <- which.min(ancestors)
    
    order[i] <- names(ancestors[m])
    
    if ( i > 1 ) {
      if ( ancestors[m] == v)
        attr(order, 'porder')[i] <- '-'
      else
        attr(order, 'porder')[i] <- '<'
    }
    
    v <- ancestors[m]
    ancestors <- ancestors[-m]
  }
  
  class(order) <- 'tsort'
  
  return(order)
}

tsort.relation_ensemble <- function(x, ...) {
  r <- lapply(x, tsort, ...)
  names(r) <- names(x)

  return(r)
}

print.tsort <- function(x) {
  cat(paste(attr(x, 'porder'), x, collapse=' '), '\n')
}
