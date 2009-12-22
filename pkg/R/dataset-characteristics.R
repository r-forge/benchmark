

o <- function(...) {
  fs <- list(...)
  function(...) Reduce(function(x, f) f(x), fs, ...)
}


p <- function(fn, args) {
  structure(list(fn = fn, args = args), class = 'p')
}


characteristics <- function() {
  structure(list(map = list(),
                 reduce = list()), class = 'characteristics')
}


print.characteristics <- function(x, ...) {
  cat(sQuote(attr(x, 'name')), 'characteristics\n')
}


summary.characteristics <- function(object, ...) {
  traverse.tree <- function(tree, level = NULL) {
    lapply(names(tree),
           function(nodename) {
             class <- class(tree[[nodename]])
             ws <- paste(rep('  ', length(level)), collapse = '')

             cat(sprintf('%s%s%s%s',
                         ws,
                         ifelse(class != 'list', '', ''),
                         nodename,
                         ifelse(class == 'list', ':', '')), '\n')

             if ( class == 'list' )
               return(traverse.tree(tree[[nodename]],
                                    c(level, nodename)))
           })
  }

  cat(sQuote(attr(object, 'name')), 'characteristics:\n\n')
  invisible(traverse.tree(object$map))
}



### Statlog characteristics:

statlog <- function() {
  library(e1071)
  library(entropy)

  ch <- characteristics()

  ### Map:
  ch$map$input <- list(n = nrow,
                       attr = ncol,
                       factor = list(attr = ncol,
                                     . = list(nlevels = nlevels,
                                              entropy = o(na.omit, as.integer,
                                                          entropy.empirical))),
                       numeric = list(attr = ncol,
                                      mac = mac,
                                      . = list(skewness = o(na.omit, skewness),
                                               kurtosis = o(na.omit, kurtosis))))

  ch$map$response <- list(factor = list(. = list(cl = nlevels,
                                                 entropy = o(na.omit, as.integer,
                                                             entropy.empirical))))

  ch$map$input2response <- list(numeric2factor = list(fcc = fcc,
                                                      frac1 = frac1),
                                factor2factor = list(. = list(mi = mi)))


  ### Reduce:
  ch$reduce$input <- list(n = identity,
                          attr = identity,
                          factor = list(attr = na0,
                                        . = list(bin = p(binary, list(c('input', 'factor', '.', 'nlevels'))),
                                                 entropy = identity,
                                                 nlevels = NULL)),
                          numeric = list(attr = na0,
                                         mac = mean,
                                         . = list(skewness = mean,
                                                  kurtosis = mean)))

  ch$reduce$response <- list(factor = list(. = list(cl = identity,
                                                    entropy = identity)))

  ch$reduce$input2response <- list(numeric2factor = list(fcc = identity,
                                                         frac1 = identity),
                                   factor2factor = list(. = list(mi = mean),
                                                        enattr = p(enattr, list(c('response',
                                                                                  'factor', '.', 'entropy'),
                                                                                c('input2response', 'factor2factor', '.', 'mi'))),
                                                        nsratio = p(nsratio, list(c('input', 'factor', '.', 'entropy'),
                                                                                  c('input2response', 'factor2factor', '.', 'mi')))))

  structure(ch, name = 'StatLog',
            class = c('statlog.characteristics', class(ch)))
}



### Implementation of needed characteristics:

na0 <- function(x) {
  ifelse(is.na(x), 0, x)
}

enc <- function(x) {
  y <- matrix(0, nrow=length(x), ncol=nlevels(x))
  y[cbind(seq(length(x)), as.numeric(x))] <- 1
  y
}

mac <- function(x) {
  x <- as.matrix(x)

  if ( ncol(x) == 1 )
    return(NA)

  drop(sapply(seq(length = ncol(x)),
              function(i)
              sqrt(summary(lm(x[, i] ~ x[, -i]))$r.squared)))
}

fcc <- function(x, y) {
  x <- as.matrix(x)
  y <- unlist(y)
  max(cancor(x, enc(y))$cor)
}

frac1 <- function(x, y) {
  x <- as.matrix(x)
  y <- unlist(y)
  cor <- cancor(x, enc(y))$cor
  lambda <- cor^2 / (1 - cor^2)

  max(lambda) / sum(lambda)
}

mi <- function(x, y) {
  mi.plugin(cbind(as.integer(x),
                  as.integer(y)))
}

binary <- function(x, ...) {
  na0(sum(x == 2))
}

enattr <- function(x, y, ...) {
  x / y
}

nsratio <- function(x, y, ...) {
  (x - y) / y
}
