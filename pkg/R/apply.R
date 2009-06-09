# Allow dimnames as MARGIN
apply <- function(X, MARGIN, FUN, ...) {

  if ( is.character(MARGIN) ) {
    d <- names(dimnames(X))
    MARGIN <- which(d == MARGIN)
  }

  eval(substitute(base::apply(X, MARGIN, FUN, ...)),
       envir=parent.frame())
}
