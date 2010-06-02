
library(proto)
library(reshape)
library(ggplot2)

be <- BenchmarkExperiment(sprintf("ds%s", 1:4), 5,
                          algorithms = sprintf("alg%s", 1:3),
                          performances = sprintf("perf%s", 1:3))

with(be, debug(viewAlgorithmPerformance))



bewh <- bewarehouse(c("ds1", "ds2"), 4,
                    algorithms = c("alg1", "alg2"),
                    performances = c("perf1"),
                    characteristics = c("char1"))

bewh$viewAlgorithmPerformance()
bewh$viewAlgorithmPerformance(datasets = "ds1")



### uci621: ##########################################################

load("uci621raw.RData")


u <- bewarehouse(c("monks3", "BreastCancer"), 250,
                 algorithms = c("lda", "rf", "knn", "rpart", "svm", "nnet"),
                 performances = c("Misclassification", "Time"))

tmp <- uci621raw[, , , "monks3"]
names(dimnames(tmp)) <- c("samples", "algorithms", "performances")
u$data$monks3$AlgorithmPerformance <- tmp

tmp <- uci621raw[, , , "BreastCancer"]
names(dimnames(tmp)) <- c("samples", "algorithms", "performances")
u$data$BreastCancer$AlgorithmPerformance <- tmp



dat <- u$viewAlgorithmPerformance()

boxplot(dat)
boxplot(subset(dat,
               datasets = "BreastCancer"))

densityplot(dat)
densityplot(subset(dat,
                   datasets = "monks3",
                   performances = "Misclassification"))

stripchart(dat)
stripchart(subset(dat,
                  datasets = "monks3",
                  performances = "Misclassification"))

pcplot(subset(dat,
              datasets = "monks3",
              performances = "Misclassification"))


dat1 <- subset(dat,
               datasets = "monks3",
               performances = "Misclassification")

a <- friedman.ibea(subset(dat,
                     datasets = "monks3",
                     performances = "Misclassification"))
a$globalTest()
a$pairwiseTest()

ga <- globalTest(a)
pa <- pairwiseTest(a)




b <- lmer.ibea(subset(dat,
                      datasets = "monks3",
                      performances = "Misclassification"))
b$model
b$pairwiseTest()
b$globalTest()

globalTest(b)
plot(pairwiseTest(b))
pb <- pairwiseTest(b)





sapply(pe, function(a) sapply(pe, function(b) a - b))




engine <- FriedmanTestPaircomp$new(dat1, "<", 0.05)
engine$decision()

engine <- FriedmanTestPaircomp$new(dat1, "=", 0.05)
engine$decision()

engine <- GenericPointPaircomp$new(dat1, "=", "mean", tolerance = 0.001)
engine$decision()

engine <- LmerTestPaircomp$new(dat1, "<", 0.05, relevance = 0.01)
engine$decision()

engine <- LmerTestPaircomp$new(dat1, "=", 0.05)
engine$decision()

engine <- PercintTestPaircomp$new(dat1, "=", 0.2)
engine$globalTest()
engine$pairwiseTest()
engine$desicion()

engine <- PercintTestPaircomp$new(dat1, "<", 0.49)
engine$desicion()

plot(engine$ci)


ci <- t(sapply(split(dat1$value, dat1$algorithms), pci, 0.05))

which.min(ci[, 'lwr'])
which.max(ci[, 'upr'])

overlap(ci[which.min(ci[, 'lwr']), ],
        ci[which.max(ci[, 'upr']), ])

