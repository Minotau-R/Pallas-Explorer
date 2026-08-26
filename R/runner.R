# -----------------------------------------------------------------------
# Top-level entry point: turns either a local/remote RDF file OR a live
# SPARQL endpoint into a schema graph + ont object in one call.
#
# Input:  `source` - a file path/URL (source_type = "file", parsed via
#         rdflib::rdf_parse()) or a SPARQL endpoint URL
#         (source_type = "endpoint", queried directly, so the dataset is
#         never fully loaded into R's memory).
# Output: a list with `graph` (igraph), `ont` (see build_ont()),
#         `prefixes`, `schema` (see extract_schema()) and `visualization`
#         (a visNetwork widget, or NULL when visualize = FALSE).
# -----------------------------------------------------------------------

#' Parse a local/remote RDF file into an in-memory rdflib model
#'
#' Thin wrapper around `rdflib::rdf_parse()`, kept separate from
#' [run_pipeline()] so the file-parsing step can be tested on its own.
#'
#' @param source A file path or URL to an RDF/OWL/VoID document.
#' @param format Passed through to `rdflib::rdf_parse()`.
#' @return A parsed `rdf` model.
#' @noRd
.parse_rdf_source <- function(source, format = "guess") {
  if (!requireNamespace("rdflib", quietly = TRUE)) {
    stop(
      "Reading from a file requires the 'rdflib' package. ",
      "Install it with install.packages('rdflib'), or pass a SPARQL ",
      "endpoint URL with source_type = 'endpoint' instead.",
      call. = FALSE
    )
  }
  rdflib::rdf_parse(source, format = format)
}



#'
#' Extracts a schema, builds its graph and ont object, and (by default)
#' renders it, from either a materialised RDF file or a live SPARQL
#' endpoint queried directly. If the graph exceeds `max_nodes`,
#' visualization is skipped (with a warning) rather than failing the
#' whole pipeline - `graph`/`ont`/`schema`/`prefixes` are still returned.
#'
#' `build_ont()` expects a `class` id column (see `schema_ont.R`), while
#' [extract_schema()] emits `uri` for classes; this function bridges that
#' shape difference so `schema_ont.R` itself stays untouched.
#'
#' @param source For `source_type = "file"` (default): a file path or URL
#'   parsed locally via `rdflib::rdf_parse()`. For `source_type =
#'   "endpoint"`: a SPARQL endpoint URL, queried directly.
#' @param title Character scalar used as the graph title; defaults to the
#'   file's base name / the endpoint URL.
#' @param format Passed to `rdflib::rdf_parse()` when `source_type = "file"`.
#' @param source_type Either `"file"` or `"endpoint"`.
#' @param prefix_source Optional SPARQL endpoint URL or `rdflib` model to
#'   query for declared prefixes, if different from `source`.
#' @param visualize If `TRUE` (default), also render the graph with
#'   [visualise_schema()] (requires the Suggested 'visNetwork' package).
#'   Set to `FALSE` to skip rendering and avoid that dependency.
#' @param max_nodes Passed to [visualise_schema()]; graphs above this size
#'   are skipped (with a warning) rather than failing the pipeline.
#' @return A list with `graph`, `ont`, `prefixes`, `schema` and
#'   `visualization` (`NULL` when `visualize = FALSE` or the graph was
#'   too large to render).
#' @export
run_pipeline <- function(source, title = NULL, format = "guess",
                         source_type = c("file", "endpoint"),
                         prefix_source = NULL,
                         visualize = TRUE,
                         max_nodes = 750) {
  source_type <- match.arg(source_type)

  if (source_type == "file") {
    rdf <- .parse_rdf_source(source, format = format)
    message("Parsed ", length(rdf), " triples.")
    query_source  <- rdf
    prefix_src    <- if (!is.null(prefix_source)) prefix_source else rdf
    default_title <- basename(source)
  } else {
    query_source  <- source
    prefix_src    <- if (!is.null(prefix_source)) prefix_source else source
    default_title <- source
  }

  schema <- extract_schema(query_source)
  g      <- build_schema_graph(schema)

  classes_for_ont <- data.frame(class = schema$classes$uri, stringsAsFactors = FALSE)
  ont <- build_ont(classes_for_ont, schema$properties)


  if (is.null(title)) title <- default_title

  vis <- NULL
  if (isTRUE(visualize)) {
    vis <- tryCatch(
      visualise_schema(g, title, max_nodes = max_nodes),
      error = function(e) {
        warning("Skipping visualization: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  }

  list(graph = g, ont = ont, schema = schema, visualization = vis)
}
