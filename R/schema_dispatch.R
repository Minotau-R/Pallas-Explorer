#' Detect ontology style and dispatch to the matching extractor
#'
#' Probes `source` for an OWL signal, then a VoID signal, and dispatches to
#' [extract_schema_owl()], [extract_schema_void()] or
#' [extract_schema_generic()] accordingly. The VoID probe is a single
#' `UNION` query (rather than two separate ones) to keep detection to at
#' most 2 HTTP round trips before real extraction starts.
#'
#' The three styles are treated as mutually exclusive: whichever signal is
#' found first wins, and the others are never inspected. This is a good fit
#' for a single ontology file, but a dataset that mixes styles (e.g. a VoID
#' description published alongside an OWL vocabulary) will only ever be
#' read through one lens, so classes/properties that only appear as plain
#' `rdf:type`/predicate usage in an otherwise-OWL graph are not merged in.
#'
#' @param source A SPARQL endpoint URL or a parsed `rdflib` compatible file.
#' @return A schema list, see [extract_schema_owl()].
#' @export
extract_schema <- function(source) {
  owl_test <- safe_query(source, '
    PREFIX owl:  <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT (COUNT(?s) AS ?n) WHERE {
      { ?s a owl:Class } UNION { ?s a rdfs:Class }
    }', expected_cols = "n")
  is_owl <- nrow(owl_test) > 0 && !is.na(owl_test$n[1]) &&
    suppressWarnings(as.numeric(owl_test$n[1])) > 0
  if (isTRUE(is_owl)) return(extract_schema_owl(source))

  void_signal <- safe_query(source, '
    PREFIX void: <http://rdfs.org/ns/void#>
    SELECT ?class ?prop WHERE {
      { [] void:class ?class }
      UNION { [] void:property ?prop }
      UNION { [] void:linkPredicate ?prop }
    } LIMIT 1', expected_cols = c("class", "prop"))
  is_void <- any(!is.na(void_signal$class)) || any(!is.na(void_signal$prop))
  if (isTRUE(is_void)) return(extract_schema_void(source))

  extract_schema_generic(source)
}
