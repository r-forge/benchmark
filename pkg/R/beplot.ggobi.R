
beplot.ggobi <- function(x, verbose=TRUE) {

  x <- x[,,,,drop=TRUE]
  
  nalgs <- ncol(x)
  algs <- colnames(x)
  reps <- factor(1:nrow(x))


  # Medals table (see table.bench): 
  ranks <- t(apply(x, 1, as.ranking, ties='random', sorted=FALSE))
  nranks <- apply(ranks, 2, function(y)table(factor(y, levels=1:nalgs)))

  # Simple rank based global algorithm order
  # (see as.ranking.medalstable):
  barranks <- rank(-colSums(x * (nalgs:1)/nalgs), ties='random')
  barorder <- order(-barranks)


  ### Plot:
  require(rggobi)
  require(ggplot2)
  
  g <- ggobi()

  if ( verbose )
    cat('windows: ')

  ## Parallel coordinates:
  perfs <- data.frame(rep=reps, x)
  
  g['perfs'] <- perfs
  
  display(g['perfs'], 'Parallel Coordinates Display',
          vars=list(X=algs[barorder]))

  if ( verbose )
    cat('parallel coordinates')


  ## Podium places:
  for ( i in seq_along(algs) ) {
    n <- paste('place', i, sep='')
    
    w <- which(ranks == i, arr.ind=TRUE)
    w <- w[order(w[,1]),]
    p <- data.frame(rep=reps,
                    perf=x[w],
                    alg=factor(algs[w[,2]], levels=algs))
  
    g[n] <- p
  
    d <- display(g[n], 'XY Plot')
    variables(d) <- list(X='alg', Y='perf')

    glyph_color(g[n]) <- unlist(p[,3])

    if ( verbose )
      cat(', ', n)
  }

  if ( verbose )
    cat('\n')

  
  invisible(g)
}
