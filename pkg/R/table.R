
table.bench <- function(x, ...) {

  nalgs <- ncol(x)
  
  # Ranks per bootstrap sample:
  ranks <- t(apply(x, 1, as.ranking, ties='random', sorted=FALSE))
  
  # Ranks per algorithm (columns=algorithms, rows=places):
  nranks <- apply(ranks, 2, function(y)table(factor(y, levels=1:nalgs)))

  
  # Medals table:
  mt <- array(dim=c(nalgs, nalgs),
              dimnames=list(places=1:nalgs, algorithms=colnames(x)))
  mt[,] <- nranks[,]

  class(mt) <- 'medalstable'

  
  return(mt)
}


print.medalstable <- function(x, ...) {
  print(unclass(x))
}


