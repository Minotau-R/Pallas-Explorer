#' Build a tab-completable "ont" object
#'
#' Converts the class/property tables discovered from a SPARQL endpoint
#' into short, unique, alphabetically-sorted names so they can be browsed
#' with regular R auto-completion, e.g. `ont$classes$Person`.
#'
#' @param classes A data frame with a `class` column of URIs, such as
#'   returned by ep_classes(). May be `NULL`.
#' @param properties A data frame with a `property` column of URIs, such as
#'   returned by ep_properties(). May be `NULL`.
#' @return A list with elements `classes` and `properties`. Each is a named
#'   list mapping a short display name to the full URI.
#' @examples
#' classes <- data.frame(class = c(
#'   "http://xmlns.com/foaf/0.1/Person",
#'   "http://schema.org/Person"
#' ))
#' ont <- build_ont(classes)
#' names(ont$classes)
#' @export
build_ont <- function(classes = NULL, properties = NULL) {
  class_uris <- if (is.null(classes)) character() else classes$class
  property_uris <- if (is.null(properties)) character() else properties$property

  list(
    classes = make_display_list(class_uris),
    properties = make_display_list(property_uris)
  )
}

#' Build a display-name -> URI lookup, disambiguating collisions
#'
#' When two different URIs share the same local name (e.g. `foaf:Person`
#' and `schema:Person`), the namespace is appended to disambiguate. In the
#' rare case that is still not enough to make names unique, the full URI is
#' appended instead.
#'
#' @param uris Character vector of URIs.
#' @return A named list mapping a unique display name to each URI in
#'   `uris` (duplicated/`NA`/empty URIs are dropped).
#' @examples
#' make_display_list(c(
#'   "http://xmlns.com/foaf/0.1/Person",
#'   "http://schema.org/Person"
#' ))
#' @export
make_display_list <- function(uris) {
  .make_display_list_impl(uris)
}

#' Build a display-name -> URI lookup, optionally preferring given labels
#'
#' The shared engine behind [make_display_list()]  When `labels` is supplied (and non-`NA`/
#' non-empty for a given id), it is used as the display name instead of the
#' id's local name -- this is what lets ontology terms show up under their
#' `rdfs:label` (e.g. `"Person"`) rather than a bare URI fragment.
#'
#' @param ids Character vector of identifiers (URIs).
#' @param labels Character vector the same length as `ids` (or a single
#'   value, recycled), or `NA_character_` (default) to always fall back to
#'   [local_name()].
#' @return A named list mapping a unique display name to each non-missing,
#'   non-empty, de-duplicated id in `ids`.
#' @noRd
.make_display_list_impl <- function(ids, labels = NA_character_) {
  ids <- as.character(ids)
  if (length(labels) != length(ids)) labels <- rep(NA_character_, length(ids))

  keep <- !is.na(ids) & nzchar(ids)
  ids <- ids[keep]
  labels <- labels[keep]
  if (length(ids) == 0) return(list())

  dup_id <- duplicated(ids)
  ids <- ids[!dup_id]
  labels <- labels[!dup_id]

  display <- ifelse(!is.na(labels) & nzchar(labels), labels, local_name(ids))
  display <- .solve_possible_duplicate_display(display, ids)

  ord <- order(display)
  stats::setNames(as.list(ids[ord]), display[ord])
}



#' This function makes sure the final output to the user has unique items.
#' In case there are duplication, the function make them unique by adding their
#' namespace. If there are equal namespaces, the function then brackets are added.
#'
#' @param display A character vector of display names to be checked for duplicates.
#' @param ids A character vector of full URIs corresponding to the display names.
#'
#' @returns display
#' @export
#'
#' @noRd
.solve_possible_duplicate_display <- function(display, ids){
  dup <- display %in% display[duplicated(display)]
  if (any(dup)) {
    display[dup] <- paste0(display[dup], "  <", namespace_of(ids[dup]), ">")
  }
  dup2 <- display %in% display[duplicated(display)]
  if (any(dup2)) {
    display[dup2] <- paste0(display[dup2], " [", ids[dup2], "]")
  }
  display
}


