

as.paircomp <- function(x, ...) {
  UseMethod('as.paircomp')
}


as.paircomp.relation_ensemble <- function(x) {
  do.call(c, lapply(x, as.paircomp))
}


as.paircomp.relation <- function(x) {
  i <- relation_incidence(x)
  ti <- t(i)

  pc <- ti[lower.tri(ti)] + (i[lower.tri(i)] * -1)

  paircomp(matrix(pc, nrow = 1),
           labels = rownames(i), mscale = c(-1, 0, 1))
}


as.paircomp.becp <- function(x) {
  stopifnot(all(dim(x)[c(1, 4)] == 1))

  x <- x[, , , , drop = TRUE]
  x <- rbind(x)

  comprow <- function(x, tri) {
      eq <- outer(x, x, '==')[tri]
      g <- outer(x, x, '>')[tri]

      r <- as.numeric(!eq)
      r[!eq & g] <- 1
      r[!eq & !g] <- -1

      r
  }

  tri <- upper.tri(matrix(nrow = ncol(x),
                          ncol = ncol(x)))

  r <- t(apply(x, 1, comprow, tri))

  paircomp(r, labels = colnames(x), mscale = c(-1, 0, 1))
}
