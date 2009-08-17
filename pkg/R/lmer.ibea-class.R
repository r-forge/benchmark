#' @include ibea-class.R
{}


#' "Mixed-effects model"-based inferential benchmark experiment
#' analysis framework.
#'
#' This framework provides the calculation of a mixed-effects
#' model based on the \code{\link{bench}} object by function
#' \code{model(bench)}.
#'
#' Based on the model, functions \code{gtest(model)} and
#' \code{ptest(model)} calculate a globa and a pairwise test
#' respectively.
#'
#' For pairwise test results, functions \code{ptest.pmatrix(test)}
#' and \code{ptest.smatrix(test)} calculate the matrix of p-values
#' and test-statistics respectively. Function
#' \code{ptest.ciarry(test, level)} calculates the confidence
#' intervals for a given significance level \code{level}.
#'
#' @export
#' @seealso \code{\link{ibea}}, \code{\link{fr.ibea-class}}
#' @aliases lmer.ibea-class
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
  ibea$gtest <- function(model) {
    stop('not yet implemented.')
  }

  ibea$ptest <- function(model) {
    return(glht(model, linfct=mcp(alg='Tukey')))
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
    s <- summary(test)

    return(structure(as.matrix(structure(s$test$pvalues,
                                         names=names(s$test$tstat)),
                               levels(test$model@frame$alg)),
                     class=c('pmatrix', 'matrix')))
  }

  ibea$ptest.smatrix <- function(test) {
    s <- summary(test)

    return(structure(as.matrix(s$test$tstat,
                               levels(test$model@frame$alg)),
                     class=c('smatrix', 'matrix')))
  }

  ibea$ptest.ciarray <- function(test, level=0.95) {
    algs <- levels(test$model@frame$alg)
    
    ci <- confint(test, level=level)
    lwr <- as.matrix(ci$confint[,'lwr'], algs)
    upr <- as.matrix(ci$confint[,'upr'], algs)

    a <- array(NA, dim=c(length(algs),length(algs),2),
               dimnames=list(algs, algs, c('lwr','upr')))
    a[,,1] <- lwr
    a[,,2] <- upr

    
    return(structure(a, class=c('ciarray', 'array')))
  }
  
  
  return(ibea)
}
