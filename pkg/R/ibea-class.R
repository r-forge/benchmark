
#' An \code{ibea} object is an environment which
#' encapsulates all methods for an individual
#' inferential analysis.
#' @title Inferential benchmark experiment analysis
#'   framework.
#' @param name The framework name
#' @export
#' @rdname ibea
#' @aliases ibea
make.ibea <- function(name='Empty') {
  ibea <- new.env(parent=emptyenv())
  ibea$name <- name
  
  class(ibea) <- 'ibea'

  return(ibea)
}

print.ibea <- function(x, ...) {
  cat(x$name, 'inferential benchmark experiment analysis framework.',
      sep=' ', fill=TRUE)
}

summary.ibea <- function(x, ...) {
  fn <- capture.output(ls.str(x, mode='function'))
  
  cat(x$name, 'inferential benchmark experiment analysis framework:',
      sep=' ', fill=TRUE)
  cat('\nAvailable functions are\n')
  cat(paste(' * ', fn, collapse='\n'))
  cat('\n')
}

