

# Test for a strict weak order.
relation_is_strict_weak_order <- function(x) {
  return(relation_is_endorelation(x) &&
         relation_is_irreflexive(x) &&
         relation_is_asymmetric(x) &&
         relation_is_transitive(x) &&
         relation_is_negatively_transitive(x))
}



relation_classes <- function (x) {
    ids <- relation_class_ids(x)
    out <- split(seq_along(ids), ids)
    class(out) <- c("relation_classes_of_objects")
    attr(out, "labels") <- names(ids)
    out
}


#  A strict weak order also has equivalence classes.
relation_class_ids <- function (x) {
    if (!is.relation(x)) 
        stop("Argument 'x' must be a relation.")
    
    if (relation_is_weak_order(x) || relation_is_strict_weak_order(x) ) {
        s <- relation_scores(x, "ranks", decreasing = FALSE)
        ids <- match(s, sort(unique(s)))
        names(ids) <- names(s)
        ids
    }
    else if (relation_is_equivalence(x)) 
        relations:::get_class_ids_from_incidence(relation_incidence(x))
    else stop("Can only determine class ids for equivalences and weak orders.")
}

