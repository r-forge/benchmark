
make.fr.ibea <- function() {

  if ( !require(coin) )
    stop('need package', sQuote('coin'))

  if ( !require(multcomp) )
    stop('need packages', sQuote('multcomp'))

  ### Friedman-based ibea framework:
  ibea <- make.ibea('Friedman-based')


  ## Tests:
  ibea$gtest <- function(bench) {
    data <- melt(bench)
    
    return(friedman_test(value ~ alg | samp, data=data))
  }

  ibea$ptest <- function(bench) {

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

  
  ## Test results:
  as.matrix <- function(values, algs) {
    nalgs <- length(algs)
    
    m <- matrix(NA, nrow=nalgs, ncol=nalgs,
                dimnames=list(algs, algs))

    for ( i in seq_along(values) ) {
      as <- strsplit(names(values[i]), ' - ', fixed=TRUE)[[1]]
      m[as[2],as[1]] <- values[i]
    }

    return(m)
  }

  
  ibea$ptest.pmatrix <- function(test) {
    return(structure(as.matrix(pvalue(test, method='single-step')[,1],
                               levels(test@statistic@x$alg)),
                     class=c('pmatrix', 'matrix')))
  }

  ibea$ptest.smatrix <- function(test) {
    return(structure(as.matrix(statistic(test, type='linear')[,1],
                               levels(test@statistic@x$alg)),
                     class=c('smatrix', 'matrix')))
  }

  
  return(ibea)
}
