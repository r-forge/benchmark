

as.paircomp <- function(x, ...) {
  UseMethod('as.paircomp')
}


as.paircomp.relation_ensemble <- function(x) {
  do.call('c', lapply(x, as.paircomp))
}


as.paircomp.relation <- function(x) {
  i <- relation_incidence(x)
  ti <- t(i)

  pc <- ti[lower.tri(ti)] + (i[lower.tri(i)] * -1)

  paircomp(matrix(pc, nrow=1), labels=rownames(i), mscale=c(-1, 0, 1))
}
