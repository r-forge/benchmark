
eulerdiagram <- function(x, ...) {  
  UseMethod('eulerdiagram')
}

eulerdiagram.relation <- function(x, ...) {

  if ( !relation_is_equivalence(x) )
    stop('must be an equivalence relation.')

  ids <- relation_class_ids(x)
  
  pie(table(ids),
      labels=lapply(split(names(ids), ids), paste, collapse=', '), ...)
}

