

boxplot.AlgorithmPerformance <- function(x, ...) {
  p <- ggplot(x, aes(algorithms, value))
  p <- p + facet_grid(performances ~ datasets, scales = "free")
  p + geom_boxplot(aes(fill = algorithms))
}



densityplot <- function(x, ...) {
  UseMethod("densityplot")
}

densityplot.AlgorithmPerformance <- function(x, ...) {
  p <- ggplot(x, aes(x = value, colour = algorithms, group = algorithms))
  p <- p + facet_grid(performances ~ datasets, scales = "free")
  p + geom_density(fill = NA)
}



pcplot <- function(x, ...) {
  UseMethod("pcplot")
}

pcplot.AlgorithmPerformance <- function(x, ...) {
  p <- stripchart(x)
  p + geom_line()
}



stripchart.AlgorithmPerformance <- function(x, ...) {
  p <- ggplot(x, aes(x = algorithms, y = value, colour = algorithms))
  p <- p + facet_grid(performances ~ datasets, scales = "free")
  p + geom_point()
}

