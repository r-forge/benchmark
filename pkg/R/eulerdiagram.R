
#' Eulerdiagram of relations.
#' @export
eulerdiagram <- function(x, ...) {  
  UseMethod('eulerdiagram')
}

#' @param x A \code{\link[relations]{relation}} object
#' @param ... Passed to underlying \code{\link[graphics]{pie}}
#'   function
#' @method eulerdiagram relation
#' @S3method eulerdiagram relation
#' @rdname eulerdiagram
eulerdiagram.relation <- function(x, ...) {

  if ( !relation_is_equivalence(x) )
    stop('must be an equivalence relation.')

  ids <- relation_class_ids(x)
  
  pie(table(ids),
      labels=lapply(split(names(ids), ids), paste, collapse=', '), ...)
}

