#' @include proto.R
#' @include test.R
{}



#' Pairwise comparison of algorithm performances
#'
#' @param x An \code{\link{AlgorithmPerformance}} object
#' @param family A \code{\link{Paircomp}} object
#' @param type Draw strict or indifference decision
#' @param ... Ignored
#' @return A \code{PaircompDecision} object; a list with the
#'   elements:
#'   \tabular{rl}{
#'     \code{decision} \tab The incidence matrix representing the
#'       pairwise comparisons\cr
#'     \code{type} \tab The decision type\cr
#'     \code{base} \tab A list with information on the decision base
#'   }
#'
#' @aliases PaircompDecision
#' @references
#'   See \emph{Eugster and Leisch (2008)} and \emph{Eugster et al. (2008)}
#'   in \code{citation("benchmark")}.
#' @rdname algperf-paircomp
#' @export
paircomp <- function(x, family, type = c("<", "="), ...) {
  type <- match.arg(type)

  engine <- do.call(family$new, c(list(x, type), list(...)))
  engine$decision()
}



### Decision family infrastructure: ##################################

PaircompDecision <- function(decision, type, base) {
  structure(list(decision = decision, type = type, base = base),
            class = c("PaircompDecision", "list"))
}



#' @nord
#' @S3method print PaircompDecision
print.PaircompDecision <- function(x, ...) {
  cat(sQuote(x$type), "decision:\n")
  print(x$decision)
}



#' Infrastructure for pairwise comparisons of algorithm performances
#'
#' Available \code{TestPaircomp} implementations:
#' \tabular{rl}{
#'   \code{FriedmanTestPaircomp} \tab Pairwise comparison based on the
#'     non parametric friedman test\cr
#'   \code{LmerTestPaircomp} \tab Pairwise comparison based on a mixed
#'     effects model (function \code{lmer} in package \code{lme4})\cr
#'   \code{PercintTestPaircomp} \tab Pairwise comparison based on the
#'     bootstrap percentile intervals
#' }
#'
#' Available \code{PointPaircomp} implementations:
#' \tabular{rl}{
#'   \code{GenericPointPaircomp} \tab Pairwise comparison based on
#'     point estimates.
#' }
#'
#' @aliases TestPaircomp PointPaircomp
#' @references
#'   See \emph{Eugster and Leisch (2008)} and \emph{Eugster et al. (2008)}
#'   in \code{citation("benchmark")}.
#' @seealso \code{\link{Test}}
#' @rdname Paircomp
Paircomp <- proto(expr = {
  name <- "Abstract pairwise comparison method"

  new <- function(., x, ...) NULL
  decision <- function(., ...) NULL
})



TestPaircomp <- proto(Paircomp, expr = {
  name <- "Abstract test based pairwise comparison method"
  test <- NULL
})



PointPaircomp <- proto(Paircomp, expr = {
  name <- "Abstract point estimate based pairwise comparison method"
})



### Implementation -- Friedman test based decision: ##################

#' @rdname Paircomp
#' @export
FriedmanTestPaircomp <- proto(TestPaircomp, expr = {
  new <- function(., x, type, significance) {
    stopifnot(FriedmanTest$requirements())

    test <- FriedmanTest$new(x)
    algorithms <- levels(x$algorithms[, drop = TRUE])

    switch(type,
           "<" = LeFriedmanTestPaircomp$proto(test = test,
                                              significance = significance,
                                              algorithms = algorithms),
           "=" = EqFriedmanTestPaircomp$proto(test = test,
                                              significance = significance,
                                              algorithms = algorithms))
  }
})

LeFriedmanTestPaircomp <- proto(FriedmanTestPaircomp, expr = {

  decision <- function(.) {
    result <- emptyLeDecision(.$algorithms)

    gt <- .$test$globalTest()
    pt <- NULL

    if ( gt$getPvalue() < .$significance ) {
      pt <- .$test$pairwiseTest()

      pval <- pt$getPvalue()
      tstat <- pt$getStatistic()

      desc <- pval < .$significance

      sigdirs <- sign(tstat[desc])
      sigpairs <- strsplit(rownames(desc)[desc], ' - ')
      sigpairs[sigdirs == 1] <- lapply(sigpairs[sigdirs == 1], rev)

      for ( p in sigpairs )
        result[p[1], p[2]] <- 1
    }

    PaircompDecision(result, "<",
                     list(globaltest = gt, pairwisetest = pt))
  }
})

EqFriedmanTestPaircomp <- proto(FriedmanTestPaircomp, expr = {

  decision <- function(.) {
    result <- emptyEqDecision(.$algorithms)

    gt <- .$test$globalTest()
    pt <- NULL

    if ( gt$getPvalue() < .$significance ) {
      pt <- .$test$pairwiseTest()

      pval <- pt$getPvalue()

      desc <- pval > .$significance
      sigpairs <- strsplit(rownames(desc)[desc], ' - ')

      for ( p in sigpairs )
        result[p[1], p[2]] <- result[p[2], p[1]] <- 1
    }

    PaircompDecision(result, "=",
                     list(globaltest = gt, pairwisetest = pt))
  }
})



### Implementation -- LMER test based decision: ######################

#' @rdname Paircomp
#' @export
LmerTestPaircomp <- proto(TestPaircomp, expr = {

  new <- function(., x, type, significance, relevance = 0) {
    stopifnot(LmerTest$requirements())

    test <- LmerTest$new(x)
    algorithms <- levels(x$algorithms[, drop = TRUE])

    switch(type,
           "<" = LeLmerTestPaircomp$proto(test = test,
                                          significance = significance,
                                          relevance = relevance,
                                          algorithms = algorithms),
           "=" = EqLmerTestPaircomp$proto(test = test,
                                          significance = significance,
                                          relevance = relevance,
                                          algorithms = algorithms))
  }
})

LeLmerTestPaircomp <- proto(LmerTestPaircomp, expr = {

  decision <- function(.) {
    result <- emptyLeDecision(.$algorithms)

    gt <- .$test$globalTest()
    pt <- NULL

    if ( gt$getPvalue() < .$significance ) {
      pt <- .$test$pairwiseTest()

      ci <- pt$getConfint(1 - .$significance)

      desc <- !(ci[, 'lwr'] < 0 & ci[, 'upr'] > 0)
      desc <- desc & !(ci[, 'lwr'] > -.$relevance & ci[, 'upr'] < .$relevance)

      sigdirs <- sign(ci[desc, 'Estimate'])
      sigpairs <- strsplit(rownames(ci)[desc], ' - ')
      sigpairs[sigdirs == 1] <- lapply(sigpairs[sigdirs == 1], rev)

      for ( p in sigpairs )
        result[p[1], p[2]] <- 1
    }


    PaircompDecision(result, "<",
                     list(model = .$test$model, globaltest = gt,
                          pairwisetest = pt, confint = ci))
  }
})

EqLmerTestPaircomp <- proto(LmerTestPaircomp, expr = {

  decision <- function(.) {
    result <- emptyEqDecision(.$algorithms)

    gt <- .$test$globalTest()
    pt <- NULL

    if ( gt$getPvalue() < .$significance ) {
      pt <- .$test$pairwiseTest()

      ci <- pt$getConfint(1 - .$significance)

      desc <- (ci[, 'lwr'] < 0 & ci[, 'upr'] > 0)
      desc <- desc | (ci[, 'lwr'] > -.$relevance & ci[, 'upr'] < .$relevance)

      sigpairs <- strsplit(rownames(ci)[desc], ' - ')

      for ( p in sigpairs )
        result[p[1], p[2]] <- result[p[2], p[1]] <- 1
    }

    PaircompDecision(result, "=",
                     list(model = .$test$model, globaltest = gt,
                          pairwisetest = pt, confint = ci))
  }
})



### Implementation -- Percentile interval based decision: ############

#' @rdname Paircomp
#' @export
PercintTestPaircomp <- proto(TestPaircomp, expr = {

  new <- function(., x, type, significance) {
    stopifnot(PercintTest$requirements())

    test <- PercintTest$new(x)
    algorithms <- levels(x$algorithms[, drop = TRUE])

    switch(type,
           "=" = EqPercintTestPaircomp$proto(test = test,
                                             significance = significance,
                                             algorithms = algorithms))
  }

  overlap <- function(., x, y) {
    unname(x['upr'] > y['lwr'])
  }
})

EqPercintTestPaircomp <- proto(PercintTestPaircomp, expr = {

  desicion <- function(.) {
    result <- emptyEqDecision(.$algorithms)

    ci <- .$test$pairwiseTest()$getConfint(1 - .$significance)

    desc <- !(ci[, 'lwr'] < 0 & ci[, 'upr'] > 0)

    sigpairs <- strsplit(rownames(ci)[desc], ' - ')

    for ( p in sigpairs )
      result[p[1], p[2]] <- result[p[2], p[1]] <- 1

    PaircompDecision(result, "=",
                     list(percint = ci))
  }
})



### Implementation -- Generic point estimate decision: ###############

#' @rdname Paircomp
#' @export
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
    estfn <- match.fun(.$estimator)

    val <- sapply(split(.$data$value, .$data$algorithms), estfn)
    pairs <- sapply(val, function(a) sapply(val, function(b) a - b))

    pairs[abs(pairs) < .$tolerance] <- 0

    result <- switch(.$type,
                     "=" = apply(pairs, c(1, 2), function(x) 0 == x),
                     "<" = apply(pairs, c(1, 2), function(x) 0 < x))

    PaircompDecision(result + 0, .$type, list(statistic = val,
                                              differences = pairs))
  }
})



### Internal functions: ##############################################

emptyLeDecision <- function(algorithms) {
  matrix(0,
         nrow = length(algorithms),
         ncol = length(algorithms),
         dimnames = list(algorithms, algorithms))
}



emptyEqDecision <- function(algorithms) {
  structure(diag(length(algorithms)),
            dimnames = list(algorithms, algorithms))
}
