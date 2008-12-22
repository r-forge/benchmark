
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

