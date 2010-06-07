
library(proto)
library(reshape)
library(ggplot2)
library(relations)

sapply(list.files("../R", pattern = ".R", full.names = TRUE), source)


### Warehouse:

bewh <- warehouse(c("ds1", "ds2"), 4,
                  algorithms = c("alg1", "alg2"),
                  performances = c("perf1"),
                  characteristics = c("char1"))

bewh$viewAlgorithmPerformance()
bewh$viewAlgorithmPerformance(datasets = "ds1")



### uci621: ##########################################################

load("uci621raw.RData")


u <- warehouse(c("monks3", "BreastCancer"), 250,
               algorithms = c("lda", "rf", "knn", "rpart", "svm", "nnet"),
               performances = c("Misclassification", "Time"))

tmp <- uci621raw[, , , "monks3"]
names(dimnames(tmp)) <- c("samples", "algorithms", "performances")
u$data$monks3$AlgorithmPerformance <- tmp

tmp <- uci621raw[, , , "BreastCancer"]
names(dimnames(tmp)) <- c("samples", "algorithms", "performances")
u$data$BreastCancer$AlgorithmPerformance <- tmp


dat <- u$viewAlgorithmPerformance()


### Visualization:

boxplot(dat)
boxplot(subset(dat,
               datasets = "monks3",
               performances = "Misclassification"),
        dependence.show = "outliers",
        order.by = min)


densityplot(dat)
densityplot(subset(dat,
                   datasets = "monks3",
                   performances = "Misclassification"))

stripchart(dat)
stripchart(subset(dat,
                  datasets = "BreastCancer",
                  performances = "Misclassification"),
           dependence.show = "all",
           order.by = function(x) 1/max(x))
stripchart(subset(dat,
                  datasets = "BreastCancer",
                  performances = "Misclassification"), order.by = mean)


beplot0(dat1)



### Inference:

dat1 <- subset(dat,
               datasets = "monks3",
               performances = "Misclassification")

engine <- FriedmanTestPaircomp$new(dat1, "<", 0.05)
engine$decision()

engine <- FriedmanTestPaircomp$new(dat1, "=", 0.05)
engine$decision()

engine <- GenericPointPaircomp$new(dat1, "<", "mean", tolerance = 0.001)
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


## User interface:
d1 <- paircomp(dat1, family = FriedmanTestPaircomp, type = "=", significance = 0.05)
d2 <- paircomp(dat1, family = LmerTestPaircomp, type = "<", significance = 0.05)
d2b <- paircomp(dat1, family = LmerTestPaircomp, type = "<", significance = 0.05, relevance = 0.01)
d3 <- paircomp(dat1, family = GenericPointPaircomp, type = "<", estimator = "mean")



### Preference relations: ############################################

r1 <- as.relation(d1)
relation_classes(r1)
as.ranking(r1)

r2 <- as.relation(d2)
r2
plot(r2)

relation_class_ids(r2)
relation_classes(r2)
as.ranking(r2)

relation_is_strict_weak_order(r2)

r3 <- as.relation(d3)
relation_class_ids(r3)
relation_classes(r3)
as.ranking(r3)



### Standard report: #################################################

a <- standard_report()
a$a
a$b
b




### Dataset object: ##################################################

ds <- as.dataset(Species ~ ., iris)
