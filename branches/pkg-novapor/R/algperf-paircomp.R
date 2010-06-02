


#paircomp(x, significance = 0.05, family = FriedmanTestPaircomp)
#paircomp(x, significance = 0.05, relevance = 0.1, family = LmerTestPaircomp)
#paircomp(x, tolerance = 1000, family = PointPaircomp)



paircomp <- function(x, family, type = c("<", "="), ...) {
  type <- match.arg(type)

  family$new(x, type, ...)$decision()
}



### Decision family infrastructure: ##################################

PaircompDecision <- function(decision, base) {
  structure(list(decision = decision, base = base),
            class = c("PaircompDecision", "list"))
}



Paircomp <- proto(expr = {
  name <- "Generic pairwise comparison method"

  new <- function(., x, ...) NULL
  decision <- function(., ...) NULL

  emptyLeDecision <- function(.) {
    matrix(0,
           nrow = length(.$algorithms),
           ncol = length(.$algorithms),
           dimnames = list(.$algorithms, .$algorithms))
  }

  emptyEqDecision <- function(.) {
    structure(diag(length(.$algorithms)),
              dimnames = list(.$algorithms, .$algorithms))
  }
})



TestPaircomp <- proto(Paircomp, expr = {
  name <- "Generic test based pairwise comparison method"

  globalTest <- function(.) NULL
  pairwiseTest <- function(.) NULL
})



PointPaircomp <- proto(Paircomp, expr = {
  name <- "Generic point estimate based pairwise comparison method"
})



### Implementation -- Friedman test based decision: ##################

FriedmanTestPaircomp <- proto(TestPaircomp, expr = {

  new <- function(., x, type, significance) {
    stopifnot(require("coin"))
    stopifnot(require("multcomp"))

    stopifnot(nlevels(x$datasets[, drop = TRUE]) == 1)
    stopifnot(nlevels(x$performances[, drop = TRUE]) == 1)

    algorithms <- levels(x$algorithms[, drop = TRUE])

    switch(type,
           "<" = LeFriedmanTestPaircomp$proto(data = x,
                                              significance = significance,
                                              algorithms = algorithms),
           "=" = EqFriedmanTestPaircomp$proto(data = x,
                                              significance = significance,
                                              algorithms = algorithms))
  }

  globalTest <- function(.) {
    friedman_test(value ~ algorithms | samples, data = .$data)
  }

  pairwiseTest <- function(.) {
    symmetry_test(value ~ algorithms | samples, data = .$data,
                  alternative = "two.sided",
                  teststat = "max",
                  xtrafo = function(d) {
                    trafo(d, factor_trafo = function(x)
                          model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")))
                  },
                  ytrafo = function(d) {
                    trafo(d, numeric_trafo = rank, block = .$data$samples)
                  })
  }
})

LeFriedmanTestPaircomp <- proto(FriedmanTestPaircomp, expr = {

  decision <- function(.) {
    result <- .$emptyLeDecision()

    gt <- .$globalTest()
    pt <- NULL

    if ( pvalue(gt) < .$significance ) {
      pt <- .$pairwiseTest()

      pval <- pvalue(pt, method = "single-step")
      tstat <- statistic(pt, type = "linear")

      desc <- pval < .$significance

      sigdirs <- sign(tstat[desc])
      sigpairs <- strsplit(rownames(desc)[desc], ' - ')
      sigpairs[sigdirs == 1] <- lapply(sigpairs[sigdirs == 1], rev)
      sigpairs <- do.call(rbind, sigpairs)

      result[sigpairs[, 1], sigpairs[, 2]] <- 1

      result
    }

    PaircompDecision(result, list(globaltest = gt,
                                  pairwisetest = pt))
  }
})

EqFriedmanTestPaircomp <- proto(FriedmanTestPaircomp, expr = {

  decision <- function(.) {
    result <- .$emptyEqDecision()

    gt <- .$globalTest()
    pt <- NULL

    if ( pvalue(gt) < .$significance ) {
      pt <- .$pairwiseTest()

      pval <- pvalue(pt, method = "single-step")

      desc <- pval > .$significance
      sigpairs <- strsplit(rownames(desc)[desc], ' - ')
      sigpairs <- do.call(rbind, sigpairs)

      result[sigpairs[, 1], sigpairs[, 2]] <- 1
      result[sigpairs[, 2], sigpairs[, 1]] <- 1

      result
    }

    PaircompDecision(result, list(globaltest = gt,
                                  pairwisetest = pt))
  }
})



### Implementation -- LMER test based decision: ######################

LmerTestPaircomp <- proto(TestPaircomp, expr = {

  new <- function(., x, type, significance, relevance = 0) {
    stopifnot(require(lme4))
    stopifnot(require(multcomp))

    stopifnot(nlevels(x$performances[, drop = TRUE]) == 1)

    model <- {
      if ( nlevels(x$datasets[, drop = TRUE]) == 1 )
        lmer(value ~ algorithms + (1 | samples), data = x)
      else
        lmer(value ~ algorithms * datasets + (1 | datasets/samples), data = x)
    }

    algorithms <- levels(x$algorithms[, drop = TRUE])

    switch(type,
           "<" = LeLmerTestPaircomp$proto(model = model,
                                          significance = significance,
                                          relevance = relevance,
                                          algorithms = algorithms),
           "=" = EqLmerTestPaircomp$proto(model = model,
                                          significance = significance,
                                          relevance = relevance,
                                          algorithms = algorithms))
  }

  globalTest <- function(.) {
    # glht(.$model, linfct = c("algorithms = 0"))
    # TODO: ask Fabian
    -Inf
  }

  pairwiseTest <- function(.) {
    glht(.$model, linfct = mcp(algorithms = "Tukey"))
  }
})

LeLmerTestPaircomp <- proto(LmerTestPaircomp, expr = {

  decision <- function(.) {
    result <- .$emptyLeDecision()

    gt <- .$globalTest()
    pt <- NULL

    if ( gt < .$significance ) {
      pt <- .$pairwiseTest()

      ci <- confint(pt, level = 1 - .$significance)$confint

      desc <- !(ci[, 'lwr'] < 0 & ci[, 'upr'] > 0)
      desc <- desc & !(ci[, 'lwr'] > -.$relevance & ci[, 'upr'] < .$relevance)

      sigdirs <- sign(ci[desc, 'Estimate'])
      sigpairs <- strsplit(rownames(ci)[desc], ' - ')
      sigpairs[sigdirs == 1] <- lapply(sigpairs[sigdirs == 1], rev)
      sigpairs <- do.call(rbind, sigpairs)

      result[sigpairs[, 1], sigpairs[, 2]] <- 1

      result
    }


    PaircompDecision(result, list(model = .$model, globaltest = gt,
                                  pairwisetest = pt, confint = ci))
  }
})

EqLmerTestPaircomp <- proto(LmerTestPaircomp, expr = {

  decision <- function(.) {
    result <- .$emptyEqDecision()

    gt <- .$globalTest()
    pt <- NULL

    if ( gt < .$significance ) {
      pt <- .$pairwiseTest()

      ci <- confint(pt, level = 1 - .$significance)$confint

      desc <- (ci[, 'lwr'] < 0 & ci[, 'upr'] > 0)
      desc <- desc | (ci[, 'lwr'] > -.$relevance & ci[, 'upr'] < .$relevance)

      sigpairs <- strsplit(rownames(ci)[desc], ' - ')
      sigpairs <- do.call(rbind, sigpairs)

      result[sigpairs[, 1], sigpairs[, 2]] <- 1
      result[sigpairs[, 2], sigpairs[, 1]] <- 1

      result
    }


    PaircompDecision(result, list(model = .$model, globaltest = gt,
                                  pairwisetest = pt, confint = ci))
  }
})



### Implementation -- Percentile interval based decision: ############

PercintTestPaircomp <- proto(TestPaircomp, expr = {

  new <- function(., x, type, significance) {
    stopifnot(nlevels(x$datasets[, drop = TRUE]) == 1)
    stopifnot(nlevels(x$performances[, drop = TRUE]) == 1)

    algorithms <- levels(x$algorithms[, drop = TRUE])

    ci <- t(sapply(split(x$value, x$algorithms), .$pci, significance))
    ci <- structure(ci, class = c("percint", class(ci)))

    switch(type,
           "<" = LePercintTestPaircomp$proto(ci = ci,
                                             significance = significance,
                                             algorithms = algorithms),
           "=" = EqPercintTestPaircomp$proto(ci = ci,
                                             significance = significance,
                                             algorithms = algorithms))
  }

  globalTest <- function(.) {
    .$overlap(.$ci[which.min(.$ci[, 'lwr']), ],
              .$ci[which.max(.$ci[, 'upr']), ])
  }

  pairwiseTest <- function(.) {
    pairs <- t(combn(rownames(.$ci), 2))

    result <- matrix(nrow = nrow(pairs), ncol = 1,
                     dimnames = list(apply(pairs, 1, paste, collapse = " - "),
                                     "Overlap"))

    for ( i in seq(length = nrow(result)) ) {
      result[i, 1] <- .$overlap(.$ci[pairs[i, 1], ],
                                .$ci[pairs[i, 2], ])
    }

    result
  }

  pci <- function(., x, significance) {
    s <- sort(x)
    B <- length(x)

    c(lwr = s[ceiling(B * significance)],
      upr = s[ceiling(B * (1 - significance))])
  }

  overlap <- function(., x, y) {
    unname(x['upr'] > y['lwr'])
  }
})

LePercintTestPaircomp <- proto(PercintTestPaircomp, expr = {

  desicion <- function(.) {
    result <- .$emptyLeDecision()

    gt <- .$globalTest()
    pt <- NULL

    if ( isTRUE(gt) ) {
      pt <- .$pairwiseTest()

      desc <- pt != TRUE
      sigpairs <- strsplit(rownames(desc)[desc], ' - ')
      sigpairs <- do.call(rbind, sigpairs)

      result[sigpairs[, 1], sigpairs[, 2]] <- 1
      result[sigpairs[, 2], sigpairs[, 1]] <- 1

      result

    }

    PaircompDesicion(result, list(percint = .$ci, globaltest = gt,
                                  pairwisetest = pt))
  }
})

EqPercintTestPaircomp <- proto(PercintTestPaircomp, expr = {

  desicion <- function(.) {
    result <- .$emptyEqDecision()

    gt <- .$globalTest()
    pt <- NULL

    if ( isTRUE(gt) ) {
      pt <- .$pairwiseTest()

      desc <- pt == TRUE
      sigpairs <- strsplit(rownames(desc)[desc], ' - ')
      sigpairs <- do.call(rbind, sigpairs)

      result[sigpairs[, 1], sigpairs[, 2]] <- 1
      result[sigpairs[, 2], sigpairs[, 1]] <- 1

      result
    }

    PaircompDesicion(result, list(percint = .$ci, globaltest = gt,
                                  pairwisetest = pt))
  }
})


plot.percint <- function(x, y = NULL, ...) {
  stopifnot(require(multcomp))

  t <- list(confint = cbind(Estimate = NA, x))
  multcomp:::plot.confint.glht(t, main = NA, xlab = NA, ...)
}



### Implementation -- Generic point estimate decision: ###############

GenericPointPaircomp <- proto(PointPaircomp, expr = {

  new <- function(., x, type, estimator, tolerance = .Machine$double.eps) {
    stopifnot(is.character(type))
    stopifnot(is.character(estimator))

    stopifnot(nlevels(x$datasets[, drop = TRUE]) == 1)
    stopifnot(nlevels(x$performances[, drop = TRUE]) == 1)

    algorithms <- levels(x$algorithms[, drop = TRUE])

    .$proto(data = x, type = type,
            algorithms = algorithms,
            estimator = estimator,
            tolerance = tolerance)
  }

  decision <- function(.) {
    estfn <- get(.$estimator)

    val <- sapply(split(.$data$value, .$data$algorithms), estfn)
    pairs <- sapply(val, function(a) sapply(val, function(b) a - b))

    pairs[abs(pairs) < .$tolerance] <- 0

    result <- switch(.$type,
                     "=" = apply(pairs, c(1, 2), function(x) 0 == x),
                     "<" = apply(pairs, c(1, 2), function(x) 0 < x))

    PaircompDecision(result + 0, list(statistic = values, differences = pairs))
  }
})

