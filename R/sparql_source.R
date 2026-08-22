#' Run a SPARQL query against a local model or a remote endpoint
#'
#' Dispatches `query` to [sparql_query()] when `source` is a single SPARQL
#' endpoint URL (a character scalar), or to `rdflib::rdf_query()` when
#' `source` is a parsed local RDF model (e.g. as returned by
#' `rdflib::rdf_parse()`). This lets every schema-extraction function work
#' unchanged whether `source` is a materialised RDF document or a live
#' SPARQL endpoint.
#'
#' @param source A SPARQL endpoint URL (character scalar), or an `rdf`
#'   model object as created by `rdflib::rdf_parse()`.
#' @param query A SPARQL query string.
#' @return A data frame of query results.
#' @examples
#' \dontrun{
#' run_sparql("https://query.wikidata.org/sparql", "SELECT * WHERE { ?s ?p ?o } LIMIT 1")
#' }
#' @export
run_sparql <- function(source, query) {
  if (is.character(source) && length(source) == 1) {
    return(sparql_query(source, query))
  }

  if (!requireNamespace("rdflib", quietly = TRUE)) {
    stop(
      "Querying a local RDF model requires the 'rdflib' package. ",
      "Install it with install.packages('rdflib'), or pass an endpoint URL instead.",
      call. = FALSE
    )
  }
  rdflib::rdf_query(source, query)
}

#' Run a SPARQL query, guaranteeing a well-shaped result
#'
#' Wraps [run_sparql()] so that a failed query (a malformed query, or a
#' temporarily unreachable endpoint) produces a warning and an empty, but
#' correctly-shaped, data frame instead of stopping the whole pipeline.
#'
#' @param source A SPARQL endpoint URL, or a local RDF model.
#' @param query A SPARQL query string.
#' @param expected_cols A character vector of column names that must be
#'   present in the returned data frame. If the query returns zero rows,
#'   the columns are created as empty character vectors.
#' @return A data frame with at least the columns in `expected_cols`.
#' @examples
#' \dontrun{
#' safe_query(
#'   "https://query.wikidata.org/sparql",
#'   "SELECT ?s WHERE { ?s a <http://nope> }"
#' )
#' }
#' @export
safe_query <- function(source, query, expected_cols = character()) {
  df <- tryCatch(
    run_sparql(source, query),
    error = function(e) {
      warning("SPARQL query failed: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  .ensure_cols(df, expected_cols)
}
