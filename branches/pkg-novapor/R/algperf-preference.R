
as.relation.PaircompDecision <- function(x, verbose = TRUE, ...) {
  r <- relation(incidence = x$decision, ...)


  if ( x$type == "=" ) {
    props <- check_indifference_preference(r)
    class <- "indiffpref"
  }
  else {
    props <- check_strict_preference(r)
    class <- "strictpref"
    r$.Meta$is_decreasing <- FALSE
  }

  r$.Meta <- c(r$.Meta,
               structure(props, names = sprintf("is_%s", names(props))))

  if ( verbose ) {
    cat(sQuote(x$type), "preference relation:\n")
    
    for ( p in names(props) ) {
      cat(sprintf("%s = %s:\n", p, props[[p]]))
      print(relation_violations(r, p, TRUE))
    }
  }

  structure(r, class = c(class, class(r)))
}



print.indiffpref <- function(x, ...) {
  cat("Indifference preference relation:\n")
  if ( relation_is_equivalence(x) )
    print(relation_classes(x))

  invisible(x)
}



print.strictpref <- function(x, ...) {
  cat("Strict preference relation:\n")
  if ( relation_is_irreflexive(x) && relation_is_asymmetric(x) &&
       relation_is_transitive(x) )
    print(as.ranking(x))

  invisible(x)
}



check_indifference_preference <- function(x) {
  list(reflexive = relation_is_reflexive(x),
       symmetric = relation_is_symmetric(x),
       transitive = relation_is_transitive(x))
}



check_strict_preference <- function(x) {
  list(irreflexive = relation_is_irreflexive(x),
       asymmetric = relation_is_asymmetric(x),
       transitive = relation_is_transitive(x),
       negatively_transitive = relation_is_negatively_transitive(x),
       trichotomous = relation_is_trichotomous(x))
}



### Patch 'relations' package: #######################################

relation_is_strict_weak_order <- function(x) {
  (relation_is_endorelation(x) &&
   relation_is_irreflexive(x) &&
   relation_is_asymmetric(x) &&
   relation_is_transitive(x) &&
   relation_is_negatively_transitive(x))
}


patch.relation_class_ids <- function (x) {
  if (!is.relation(x)) 
    stop("Argument 'x' must be a relation.")
  if (!identical(relation_is_crisp(x), TRUE)) 
    stop("Argument 'x' must be a crisp relation with no missings.")
  if (relation_is_weak_order(x) || relation_is_strict_weak_order(x)) {
    s <- relation_scores(x, "ranks", decreasing = FALSE)
    ids <- match(s, sort(unique(s)))
    names(ids) <- names(s)
    ids
  }
  else if (relation_is_equivalence(x)) 
    get_class_ids_from_incidence(relation_incidence(x))
  else stop("Can only determine class ids for equivalences and weak orders.")
}

environment(patch.relation_class_ids) <- getNamespace("relations")
assignInNamespace("relation_class_ids", patch.relation_class_ids, "relations")
detach("package:relations")
library(relations)
