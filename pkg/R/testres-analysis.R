

contsigseq <- function(x, significance) {
  
  ## Make codetools (R CMD check) happy:
  tests <- value <- NULL

  x <- subset(x, subset = tests == "pvalue", select = value)
  x <- x < significance

  i <- length(x)
  while ( x[i] == TRUE )
    i <- i - 1

  if ( i == length(x) )
    return(NA)

  i + 1
}


