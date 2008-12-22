
make.fr.ibea <- function() {

  if ( !require(coin) )
    stop('need package', sQuote('coin'))

  if ( !require(multcomp) )
    stop('need packages', sQuote('multcomp'))


  ### Lmer ibea framework:
  ibea <- make.ibea('Friedman-based')


  ## Tests:
  ibea$test.global <- function(bench) {
    data <- melt(bench)
    
    return(friedman_test(value ~ alg | samp, data=data))
  }

  ibea$test.pairwise <- function(bench) {

    if ( dim(bench)[3] > 1 | dim(bench)[4] > 1 )
      stop('only one performance measure on one data set is handled.')
  
    data <- melt(bench)
  
    rtt <- symmetry_test(value ~ alg | samp, data=data,
                         alternative="two.sided",
                         teststat="max",
                         xtrafo=function(d) {
                           trafo(d, factor_trafo=function(x) 
                                 model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")))
                         },
                         ytrafo=function(d) {
                           trafo(d, numeric_trafo=rank, block=data$samp)
                         })

    return(rtt)
  }

  ## Relations:
  ibea$relation.pairwise <- function(test, alpha) {
  
    p <- list(pvalues=pvalue(test, method='single-step')[,1],
              tstats=statistic(test, type = 'linear')[,1],
              alpha=alpha,
              algs=levels(test@statistic@x$alg))
    class(p) <- 'tpairs'

    return(as.relation(p))
  }

  ## All-in-one:
  ibea$relation <- function(x, alpha) {
      t <- ibea$test.pairwise(x)
      r <- ibea$relation.pairwise(t, alpha)

      return(r)
  }
    

  return(ibea)
}
