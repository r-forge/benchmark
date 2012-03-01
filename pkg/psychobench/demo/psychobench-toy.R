#' @demo Artificial toy example to illustrate each step of the
#'   psychobench methodology

library("benchmark")



### Datasets: ########################################################

dgp <- function(n) {
  n1 <- n / 4

  x1 <- c(runif(n1, min = -2, max = -1),
          runif(n1, min = -1, max = 0),
          runif(n1, min = 0, max = 1),
          runif(n1, min = 1, max = 2))
  x2 <- runif(n, min = -2, max = 2)

  c1 <- as.factor(rep(c(0, 1), c(n1 + n1, n1 + n1)))
  c2 <- as.factor(rep(c(0, 1, 1, 0), c(n1, n1, n1, n1)))

  list(linsep = data.frame(x1 = x1, x2 = x2, classes = c1),
       nlinsep = data.frame(x1 = x1, x2 = x2, classes = c2))
}

set.seed(1234)
ds <- dgp(400)

ds1 <- ds$linsep
ds2 <- ds$nlinsep


op <- par(mfrow = c(1, 2))
plot(x2 ~ x1, data = ds1, col = as.integer(ds1$classes) + 1)
plot(x2 ~ x1, data = ds2, col = as.integer(ds2$classes) + 1)
par(op)



### Algorithms and performances: #####################################

library("MASS")
library("e1071")

predict.lda <- function(object, newdata, ...) {
  MASS:::predict.lda(object, newdata, ...)$class
}

predict.qda <- function(object, newdata, ...) {
  MASS:::predict.qda(object, newdata, ...)$class
}

miscl <- function(yhat, y) {
  1 - classAgreement(table(yhat, y))$diag
}



### Dataset characterization: ########################################

ds1 <- as.dataset(classes ~ ., ds1)
ds2 <- as.dataset(classes ~ ., ds2)

rbind(characterize(ds1, StatlogCharacteristics),
      characterize(ds2, StatlogCharacteristics))



### Benchmark experiment: ############################################

set.seed(1234)
be <- benchmark(datasets = c(ds1, ds2),
                sampling = sub.sampling(100, 2/3),
                algorithms = c(svm, lda, qda),
                performances = miscl,
                characteristics = StatlogCharacteristics)
be


### Algorithm performances:

ap <- be$viewAlgorithmPerformance()

op <- par(mfrow = c(1, 2))
boxplot(value ~ algorithms, data = subset(ap, datasets == "ds1"), ylim = c(0, 0.8))
boxplot(value ~ algorithms, data = subset(ap, datasets == "ds2"), ylim = c(0, 0.8))
par(op)


### Dataset characterizations:

ch <- be$viewDatasetCharacterization()

plot(subset(ch, datasets == "ds1"), lines = FALSE)
plot(subset(ch, datasets == "ds2"), lines = FALSE)


### Tabular result:

as.psychobench(be, comparisons = FALSE)



### Preference scaling: ##############################################

library("psychotree")

pc <- as.psychobench(be)

## Note that the bttree call shows an error, which is also
## handled by the function!
t <- bttree(preference ~ ., data = pc)

plot(t)

