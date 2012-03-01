#' @demo UCI application example
#' @description Please note that the execution of the benchmark
#'   experiment is a very time consuming task. Therefore, in
#'   this demo we only present the analysis of the precalculated
#'   benchmark experiment.

library("benchmark")



### Precalculated benchmark experiment result: #######################

data("wuci", package = "psychobench")
wuci



### Preference scaling: ##############################################

pc0 <- as.psychobench(wuci)
str(pc0)



### Dataset characterization cleaning: ###############################

library("classInt")


categorize <- function(x) {
  y <- cut(x, breaks = classIntervals(x)$brks)
  levels(y) <- c('NA', levels(y))
  y[is.na(y)] <- 'NA'
  y
}

pc <- within(pc0, {
  obs.n <- ordered(input.n)
  var.n <- ordered(input.attr)
  nvar.n <- ordered(input.factor.attr)
  nvar.bin <- ordered(input.factor...bin)
  cvar.n <- ordered(input.numeric.attr)
  resp.cl <- ordered(response.factor...cl)
  nvar.entropy <- categorize(input.factor...entropy)
  cvar.mac <- categorize(input.numeric.mac)
  cvar.skew <- categorize(input.numeric...skewness)
  cvar.kurt <- categorize(input.numeric...kurtosis)
  resp.entropy <- categorize(response.factor...entropy)
  i2r.fcc <- categorize(input2response.numeric2factor.fcc)
  i2r.frac1 <- factor(input2response.numeric2factor.frac1)
  i2r.mi <- categorize(input2response.factor2factor.mi)
  i2r.envar <- categorize(input2response.factor2factor.enattr)
  i2r.nsratio <- categorize(input2response.factor2factor.nsratio)
})



### Recursive partitioning of BT-models: #############################

library("psychotree")


## Vary, e.g., with the minsplit criterium to see its
## impact on the resulting tree (minsplit = 200 results
## in the tree showed in the paper).
tree <- bttree(preference ~ ., data = pc, minsplit = 200)
tree

plot(tree)
