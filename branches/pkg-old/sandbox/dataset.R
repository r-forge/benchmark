
library(benchmark)

source('dataset.R')
source('dataset-characteristics.R')
source('dataset-characterization.R')

data(iris)
ds1 <- as.dataset(Species ~ ., iris)
ds.ch1 <- characterize(ds1, statlog())


data(airquality)
ds2 <- as.dataset(Ozone ~ ., airquality)
ds.ch2 <- characterize(ds2, statlog())

ch <- c(iris = ds.ch1,
        airquality = ds.ch2)



rx <- apply(ch, 2L, range, na.rm = TRUE)
x <- apply(ch, 2L, function(x) (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))



source('benchmark.R')

predict.lda <- function(object, newdata, ...)
  MASS:::predict.lda(object, newdata, ...)$class

miscl <- function(yhat, y)
  1 - classAgreement(table(yhat, y))$diag

set.seed(1234)
ch <- benchmark(Species ~ ., iris, 10, c(lda = lda, svm = svm), miscl)



library(MASS)
par(las = 2, mar = c(10, 0, 0, 0))
parcoord(as.matrix(ch$dataset))


library(ggplot2)

d <- melt(as.matrix(scale(ch$dataset)))
qplot(X2, value, data = d, geom = c('point', 'line'), group = X1)


m <- ch$bench[,,,,drop=TRUE]
m <- cbind(m, runif(10))


tri <- lower.tri(matrix(NA, nrow = 3, ncol = 3))
eq <- t(outer(m[1, ], m[1, ], '=='))[tri]
g <- t(outer(m[1, ], m[1, ], '>'))[tri]
