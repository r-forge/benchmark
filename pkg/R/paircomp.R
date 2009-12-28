

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
  n <- ncol(x)
  tri <- lower.tri(matrix(nrow = n, ncol = n))

  r <- t(sapply(seq(length = nrow(x)),
                function(i) {
                  eq <- t(outer(x[i, ], x[i, ], '=='))[tri]
                  g <- t(outer(x[i, ], x[i, ], '>'))[tri]

                  r <- as.numeric(!eq)
                  r[!eq & g] <- 1
                  r[!eq & !g] <- -1
                  r
                }))

  if ( nrow(r) != nrow(x) )
    r <- t(r)

  paircomp(r, labels = colnames(x), mscale = c(-1, 0, 1))
}
