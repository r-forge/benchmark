


### Test procedure infrastructure: ###################################

#' Infrastructure for test procedures.
#'
#' Available \code{Test} and corresponding \code{TestResult}
#' implementations:
#' \tabular{rl}{
#'   \code{FriedmanTest} \tab Test procedure based on the
#'     non parametric friedman test\cr
#'   \code{LmerTest} \tab Test procedure based on a mixed
#'     effects model (function \code{lmer} in package \code{lme4})\cr
#'   \code{PercintTest} \tab Test procedure based on the
#'     bootstrap percentile intervals
#' }
#'
#' @references
#'   See \emph{Eugster and Leisch (2008)} and \emph{Eugster et al. (2008)}
#'   in \code{citation("benchmark")}.
#' @rdname Test
Test <- proto(expr = {
  requirements <- function(., ...) NULL
  new <- function(., ...) NULL
  globalTest <- function(., ...) NULL
  pairwiseTest <- function(., ...) NULL
})



TestResult <- proto(expr = {
  new <- function(., ...) NULL
  getPvalue <- function(., ...) NULL
  getStatistic <- function(., ...) NULL
  getConfint <- function(., ...) NULL
})



### Implementation -- Friedman test: #################################

#' @rdname Test
#' @export
FriedmanTest <- proto(Test, expr = {

  requirements <- function(.) {
    stopifnot(require("coin"))
    stopifnot(require("multcomp"))

    TRUE
  }

  new <- function(., data) {
    stopifnot(nlevels(data$datasets[, drop = TRUE]) == 1)
    stopifnot(nlevels(data$performances[, drop = TRUE]) == 1)

    .$proto(data = data)
  }

  globalTest <- function(.) {
    t <- friedman_test(value ~ algorithms | samples, data = .$data)

    FriedmanGlobalTestResult$new(t)
  }

  pairwiseTest <- function(.) {
    t <- symmetry_test(value ~ algorithms | samples, data = .$data,
                       alternative = "two.sided",
                       teststat = "max",
                       xtrafo = function(d) {
                         trafo(d, factor_trafo = function(x)
                               model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")))
                       },
                       ytrafo = function(d) {
                         trafo(d, numeric_trafo = rank, block = .$data$samples)
                       })

    FriedmanPairwiseTestResult$new(t)
  }
})



FriedmanGlobalTestResult <- proto(TestResult, expr = {
  new <- function(., test) {
    .$proto(test = test)
  }

  getPvalue <- function(.) {
    pvalue(.$test)
  }

  getStatistic <- function(.) {
    statistic(.$test)
  }
})



FriedmanPairwiseTestResult <- proto(TestResult, expr = {
  new <- function(., test) {
    .$proto(test = test)
  }

  getPvalue <- function(.) {
    if ( nlevels(.$test@statistic@x$algorithms) == 2 )
      .$.pvalue()
    else
      pvalue(.$test, method = "single-step")
  }

  getStatistic <- function(.) {
    if ( nlevels(.$test@statistic@x$algorithms) == 2 )
      .$.statistic()
    else
      statistic(.$test, type = "linear")
  }

  .pvalue <- function(.) {
    ret <- as.matrix(pvalue(.$test))
    rownames(ret) <- paste(rev(levels(.$test@statistic@x$algorithms)), collapse = " - ")
    colnames(ret) <- ""

    ret
  }

  .statistic <- function(.) {
    ret <- as.matrix(statistic(.$test))
    rownames(ret) <- paste(rev(levels(.$test@statistic@x$algorithms)), collapse = " - ")
    colnames(ret) <- ""

    ret
  }
})



### Implementation -- Lmer test: #####################################

#' @rdname Test
#' @export
LmerTest <- proto(Test, expr = {

  requirements <- function(.) {
    stopifnot(require("lme4"))
    stopifnot(require("multcomp"))

    TRUE
  }

  new <- function(., data) {
    stopifnot(nlevels(data$performances[, drop = TRUE]) == 1)

    model <- {
      if ( nlevels(data$datasets[, drop = TRUE]) == 1 )
        lmer(value ~ algorithms + (1 | samples), data = data)
      else
        lmer(value ~ algorithms * datasets + (1 | datasets/samples), data = data)
    }

    .$proto(model = model)
  }

  globalTest <- function(.) {
    K <- diag(length(fixef(.$model)))[-1, , drop = FALSE]
    rownames(K) <- names(fixef(.$model))[-1]

    t <- glht(.$model, linfct = K)

    LmerGlobalTestResult$new(t)
  }

  pairwiseTest <- function(.) {
    t <- glht(.$model, linfct = mcp(algorithms = "Tukey"))

    LmerPairwiseTestResult$new(t)
  }
})



LmerGlobalTestResult <- proto(TestResult, expr = {
  new <- function(., test) {
    .$proto(test = test)
  }

  getPvalue <- function(.) {
    as.numeric(summary(.$test, test = Chisqtest())$test$pvalue)
  }

  getStatistic <- function(.) {
    as.numeric(summary(.$test, test = Chisqtest())$test$fstat)
  }
})



LmerPairwiseTestResult <- proto(TestResult, expr = {
  new <- function(., test) {
    .$proto(test = test)
  }

  getPvalue <- function(.) {
    s <- summary(.$test)
    ret <- as.matrix(s$test$pvalues)
    rownames(ret) <- names(s$test$tstat)
    colnames(ret) <- ""

    ret
  }

  getStatistic <- function(.) {
    s <- summary(.$test)
    ret <- as.matrix(s$test$tstat)
    rownames(ret) <- names(s$test$tstat)
    colnames(ret) <- ""

    ret
  }

  getConfint <- function(., significance) {
    confint(.$test, level = 1 - significance)$confint
  }
})



### Implementation -- Percentile interval test: ######################

#' @rdname Test
#' @export
PercintTest <- proto(Test, expr = {
  requirements <- function(.) {
    TRUE
  }

  new <- function(., data) {
    stopifnot(nlevels(data$datasets[, drop = TRUE]) == 1)
    stopifnot(nlevels(data$performances[, drop = TRUE]) == 1)

    .$proto(data = data)
  }

  pairwiseTest <- function(.) {
    PercintPairwiseTestResult$new(data = .$data)
  }
})


PercintPairwiseTestResult <- proto(TestResult, expr = {
  new <- function(., data) {
    .$proto(data = data)
  }

  getConfint <- function(., significance) {
    pci <- .$percint(significance)
    pairs <- t(combn(rownames(pci), 2))

    ret <- matrix(nrow = nrow(pairs), ncol = 3)
    rownames(ret) <- apply(pairs, 1, paste, collapse = " - ")
    colnames(ret) <- c("Estimate", "lwr", "upr")

    for ( i in seq(length = nrow(pairs)) ) {
      ret[i, 1] <- NA
      ret[i, 2] <- pci[pairs[i, 1], 1] - pci[pairs[i, 2], 1]
      ret[i, 3] <- pci[pairs[i, 1], 2] - pci[pairs[i, 2], 2]
    }

    ret
  }

  percint <- function(., significance) {
    ci <- t(sapply(split(.$data$value, .$data$algorithms), .$.pci, significance))
    structure(ci, class = c("percint", class(ci)))
  }

  .pci <- function(., x, significance) {
    s <- sort(x)
    B <- length(x)

    c(lwr = s[ceiling(B * significance)],
      upr = s[ceiling(B * (1 - significance))])
  }
})



plot.percint <- function(x, y = NULL, ...) {
  stopifnot(require("multcomp"))

  t <- list(confint = cbind(Estimate = NA, x))
  multcomp:::plot.confint.glht(t, main = NA, xlab = NA, ...)
}
