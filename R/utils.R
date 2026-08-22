#' Namespace portion of a URI
#'
#' Extracts the "namespace" portion of a URI, keeping whichever delimiter
#' (`/` or `#`) was actually used, so it can be compared directly against
#' known vocabulary namespace strings.
#'
#' @param uri Character vector of URIs.
#' @return A character vector the same length as `uri`.
#' @examples
#' namespace_of("http://xmlns.com/foaf/0.1/Person")
#' namespace_of("http://www.w3.org/2002/07/owl#Class")
#' @export
namespace_of <- function(uri) {
  ifelse(is.na(uri), NA_character_, sub("([/#])[^/#]*$", "\\1", uri))
}

#' Short display name for a URI
#'
#' Returns the fragment or last path segment of a URI, falling back to the
#' full URI when no `/` or `#` delimiter is present.
#'
#' @param uri Character vector of URIs.
#' @return A character vector the same length as `uri`.
#' @examples
#' local_name("http://xmlns.com/foaf/0.1/Person")
#' local_name("http://www.w3.org/2002/07/owl#Class")
#' @export
local_name <- function(uri) {
  nm <- sub(".*[/#]", "", uri)
  ifelse(is.na(uri), NA_character_, ifelse(nzchar(nm), nm, uri))
}

#' Normalise a URI's scheme
#'
#' Rewrites `https://` to `http://` so namespace comparisons aren't
#' scheme-sensitive (many vocabularies are inconsistently referenced with
#' either scheme).
#'
#' @param x Character vector of URIs.
#' @return A character vector the same length as `x`.
#' @examples
#' normalize_scheme("https://schema.org/")
#' @export
normalize_scheme <- function(x) sub("^https://", "http://", x)


.empty_schema <- function() {
  list(
    classes = data.frame(uri = character(), label = character(), stringsAsFactors = FALSE),
    properties = data.frame(property = character(), label = character(), stringsAsFactors = FALSE),
    subclass = data.frame(child = character(), parent = character(), stringsAsFactors = FALSE),
    property_domains = data.frame(property = character(), domain = character(), stringsAsFactors = FALSE),
    property_ranges = data.frame(property = character(), range = character(), stringsAsFactors = FALSE),
    extra_edges = data.frame(from = character(), to = character(), relation = character(), stringsAsFactors = FALSE),
    union_intersection = data.frame(from = character(), member = character(), relation = character(), stringsAsFactors = FALSE),
    restrictions = data.frame(restriction = character(), onProperty = character(), target = character(),
                              relation = character(), cardinality_label = character(), stringsAsFactors = FALSE)
  )
}

#' @noRd
.ensure_cols <- function(df, expected_cols) {
  if (is.null(df) || !is.data.frame(df)) df <- data.frame()
  if (nrow(df) == 0) {
    return(as.data.frame(
      setNames(rep(list(character(0)), length(expected_cols)), expected_cols),
      stringsAsFactors = FALSE
    ))
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (col in expected_cols) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }
  df
}
