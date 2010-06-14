

#' @importFrom ggplot2 print.proto
{}
#print.proto <- function(x, ...) {
#   x$pprint(...)
# }



#' @importFrom ggplot2 pprint
{}
# pprint <- function (x, ...) {
#   print(as.list(x), ...)
# }



#' @S3method summary proto
#' @nord
summary.proto <- function(object, ...) {
  object$psummary(...)
}

