
as.ranking <- function(x, ...) {
  UseMethod('as.ranking')
}

as.ranking.tsort <- function(x, ...) {
  r <- numeric(length=length(x))
  names(r) <- x
  r[1] <- 1

  porder <- attr(x, 'porder')

  a <- 1
  for ( i in 2:length(x) ) {
    if ( porder[i] == '<' )
      a <- i
      
    r[i] <- a    
  }

  # Handle ties and sort:
  r <- as.ranking.default(r, ...)
  
  class(r) <- 'ranking'

  return(r)
}

as.ranking.default <- function(x, ties='min', sorted=TRUE, ...) {
  r <- rank(x, ties.method=ties, ...)

  if ( sorted )
    r <- r[order(r)]

  class(r) <- 'ranking'

  return(r)
}

as.ranking.medalstable <- function(x, ties='min', sorted=TRUE, ...) {
  nalgs <- ncol(x)
  r <- rank(-colSums(x * (nalgs:1)/nalgs), ties=ties)

  if ( sorted )
    r <- r[order(r)]

  class(r) <- 'ranking'

  return(r)
}

print.ranking <- function(x) {
  print(unclass(x))
}
