

bttree <- function(x, ...) {
  UseMethod('bttree')
}


bttree.default <- function(x, ...) {
  psychotree::bttree(x, ...)
}


bttree.psychobench <- function(x, ...) {
  data <- as.data.frame(x$dataset)
  data$preferences <- as.paircomp(x$bench)

  bttree(preferences ~ ., data = data, ...)
}


