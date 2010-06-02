

Ibea <- proto(expr = {
  name <- "Generic IBEA"
  data <- NULL
})



as.ibea <- function(x) {
  structure(x, class = c("ibea", class(x)))
}



globalTest <- function(x, ...) {
  UseMethod("globalTest")
}

globalTest.ibea <- function(x, ...) {
  x$globalTest()
}



pairwiseTest <- function(x, ...) {
  UseMethod("pairwiseTest")
}

pairwiseTest.ibea <- function(x, ...) {
  x$pairwiseTest()
}



### Friedman-based: ##################################################

FriedmanIbea <- proto(Ibea, expr = {
  name <- "Friedman IBEA"

  globalTest <- function(.) {
    friedman_test(value ~ algorithms | samples, data = .$data)
  }

  pairwiseTest <- function(.) {
    symmetry_test(value ~ algorithms | samples, data = .$data,
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
})



friedman.ibea <- function(x) {

  stopifnot(require("coin"))
  stopifnot(require("multcomp"))

  stopifnot(nlevels(x$datasets[, drop = TRUE]) == 1)
  stopifnot(nlevels(x$performances[, drop = TRUE]) == 1)

  as.ibea(FriedmanIbea$proto(data = x))
}


paircomp.glht <- function(x, significance = 0.05) {
  p <- pvalue(p, method = "single-step")


}



### LMER-based: ######################################################

LmerIbea <- proto(Ibea, expr = {
  name <- "LMER IBEA"
  model <- NULL

  globalTest <- function(.) {
    # glht(.$model, linfct = c("algorithms = 0"))
    # TODO: Fabian oder Torsten
  }

  pairwiseTest <- function(.) {
    glht(.$model, linfct = mcp(algorithms = "Tukey"))
  }
})



lmer.ibea <- function(x) {

  stopifnot(require(lme4))
  stopifnot(require(multcomp))

  stopifnot(nlevels(x$performances[, drop = TRUE]) == 1)

  model <- {
    if ( nlevels(x$datasets[, drop = TRUE]) == 1 )
      lmer(value ~ algorithms + (1 | samples), data = x)
    else
      lmer(value ~ algorithms * datasets + (1 | datasets/samples), data = x)
  }

  as.ibea(LmerIbea$proto(data = x, model = model))
}




