


print.proto <- function(x, ...) {
  x$pprint(...)
}



pprint <- function (x, ...) {
  print(as.list(x), ...)
}



#' @S3method summary proto
#' @nord
summary.proto <- function(object, ...) {
  object$psummary(...)
}

