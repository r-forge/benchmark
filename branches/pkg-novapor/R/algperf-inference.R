

friedman <- function() {

  stopifnot(require("coin"))
  stopifnot(require("multcomp"))
  
  
  p <- proto(name = "Friedman-based")

  p$globalTest <- function(x) {    
    friedman_test(value ~ algorithms | samples, data = x)
  }

  p$pairwiseTest <- function(x) {
    symmetry_test(value ~ algorithms | samples, data = x,
                  alternative = "two.sided",
                  teststat = "max",
                  xtrafo = function(d) {
                    trafo(d, factor_trafo=function(x) 
                          model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")))  
                  },
                  ytrafo = function(d) {
                    trafo(d, numeric_trafo=rank, block=data$samp)
                  })
  }

  p
}


