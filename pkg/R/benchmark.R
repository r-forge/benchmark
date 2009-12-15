

benchmark <- function(formula, data, B, algorithms, performance,
                      data.characterize = TRUE, data.characteristics = statlog()) {

  n <- nrow(data)

  L.index <- lapply(1:B, function(.) sample(1:n, replace = TRUE))
  T.index <- lapply(L.index, function(b) setdiff(1:n, b))

  if ( data.characterize ) {
    dataset <- as.dataset(formula, data)
    dataset.ch <- characterize(dataset, data.characteristics)
  }

  data <- model.frame(formula, data = data)
  response <- attr(attributes(data)$terms, 'response')
  attr(data, 'terms') <- NULL

  nalgs <- length(algorithms)
  bench <- matrix(NA, ncol = nalgs, nrow = B,
                  dimnames = list(NULL, names(algorithms)))

  for ( i in seq(length = B) ) {
    if ( data.characterize )
      dataset.ch <- c(dataset.ch,
                      characterize(dataset, data.characteristics,
                                   index = L.index[[i]]))

    for ( j in seq(length = nalgs) ) {
      m <- algorithms[[j]](formula, data = data[L.index[[i]], ])
      p <- predict(m, newdata = data[T.index[[i]], -response])

      bench[i, j] <- performance(p, data[T.index[[i]], response])
    }
  }


  if ( data.characterize )
    return(list(bench = as.bench(bench),
                dataset = dataset.ch[-1, ]))

  return(as.bench(bench))
}

