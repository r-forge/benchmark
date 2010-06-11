

plot.DatasetCharacterization <- function(x, y = NULL, lines = TRUE, points = TRUE,
                                         null.line = TRUE, null.line.col = gray(0.7),
                                         ...) {

  stopifnot(nlevels(x$datasets[, drop = TRUE]) == 1)

  data <- dcscale(x)


  p <- ggplot(data, aes(characteristics, value, group = samples))

  if ( null.line )
    p <- p + geom_hline(aes(yintercept = 0), colour = null.line.col)

  if ( lines )
    p <- p + geom_line()

  if ( points )
    p <- p + geom_point()

  p <- p + scale_y_continuous('', breaks = c(-0.2, seq(0, 1, by = 0.2)),
                              labels = c("NA", seq(0, 1, by = 0.2))) +
           scale_x_discrete("Characteristics") +
           theme_update(axis.text.x = theme_text(angle = 90, hjust = 1))

  p
}



plot.DatasetBasisCharacterization <- function(x, y) {

}



### Internal functions: ##############################################

dcscale <- function(x) {
  x0 <- x

  x <- do.call(cbind, split(x$value, x$characteristics))
  rx <- apply(x, 2L, range, na.rm = TRUE)
  sx <- apply(x, 2L,
              function(x)
              (x - min(x, na.rm = TRUE)) /
              (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))

  sx[is.na(x)] <- -0.2

  m <- matrix(FALSE, nrow = nrow(x), ncol = ncol(x))
  m[, apply(rx, 2, function(x) length(unique(x)) == 1)] <- TRUE
  sx[m] <- 1

  y <- merge(x0, melt(sx),
             by.x = c("samples", "characteristics"),
             by.y = c("X1", "X2"), sort = FALSE)
  y$value.x <- NULL
  y$value <- y$value.y
  y$value.y <- NULL

  y[, c("samples", "datasets", "characteristics", "value")]
}

