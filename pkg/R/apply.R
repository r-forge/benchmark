
apply <- function(X, MARGIN, FUN, ...) {

  if ( is.character(MARGIN) ) {
    d <- names(dimnames(X))
    MARGIN <- which(d == MARGIN)
  }

  eval(substitute(base::apply(X, MARGIN, FUN, ...)),
       envir=parent.frame())
}


fapply <- function(X, MARGIN, FUN, ...) {

  ### lattice:::bwplot.formula code:
  forumla <- MARGIN
  form <- latticeParseFormula(formula, X)

  cond <- form$condition
  x <- form$right
  y <- form$left

  if (is.null(y)) {
    y <- rep(if (is.null(names(x))) "" else names(x), length = length(x))
    y <- factor(y, levels = unique(y))
  }

  if (length(cond) == 0) {
    strip <- FALSE
    cond <- list(gl(1, length(x)))
  }


  
  
}
