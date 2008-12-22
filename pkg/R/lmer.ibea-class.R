
make.lmer.ibea <- function() {

  if ( !require(lme4) )
    stop('need package', sQuote('lme4'), '.')
  
  if ( !require(multcomp) )
    stop('need package', sQuote('multcomp'), '.')

  
  ### Lmer ibea framework:
  ibea <- make.ibea('Lmer')

  ## Model:
  ibea$model <- function(bench) {
    if ( dim(bench)[3] > 1 )
      stop('only one performance measure is handled.')
    
    data <- melt(bench)
    
    model <- {
      if ( dim(bench)[4] > 1 )
        lmer(value ~ alg * ds + (1 | ds/samp), data=data)
      else
        lmer(value ~ alg + (1 | samp), data=data)
    }
  
    return(model)
  }

  ## Tests:
  ibea$test.global <- function(model) {
    stop('not yet implemented.')
  }

  ibea$test.pairwise <- function(model) {
    return(glht(model, linfct=mcp(alg='Tukey')))
  }

  ## Relations:
  ibea$relation.pairwise <- function(test, alpha) {
    s <- summary(test)

    p <- list(pvalues=s$test$pvalues,
              tstats=s$test$tstat,
              alpha=alpha,
              algs=levels(test$model@frame$alg))
    attr(p$pvalues, 'names') <- attr(p$tstat, 'names')
    class(p) <- 'tpairs'

    return(as.relation(p))
  }

  ## All-in-one:
  ibea$relation <- function(x, alpha) {
      m <- ibea$model(x)
      t <- ibea$test.pairwise(m)
      r <- ibea$relation.pairwise(t, alpha)

      return(r)
  }
  

  return(ibea)
}
