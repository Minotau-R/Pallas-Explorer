#' @importFrom dplyr %>% filter transmute distinct
NULL


#' Extract a schema from a VoID dataset description,extracted information
#'
#' @param source A SPARQL endpoint URL or a parsed rdflib file.
#' @return A schema list, see `.empty_schema()`; only `classes` and
#'   `properties` are populated.
#' @export
extract_schema_void <- function(source) {
  schema <- .empty_schema()

  void_classes <- safe_query(source, '
    PREFIX void: <http://rdfs.org/ns/void#>
    SELECT DISTINCT ?class WHERE { [] void:class ?class . }', expected_cols = "class")

  void_props <- safe_query(source, '
    PREFIX void: <http://rdfs.org/ns/void#>
    SELECT DISTINCT ?prop WHERE {
      { [] void:property ?prop } UNION { [] void:linkPredicate ?prop }
    }', expected_cols = "prop")

  schema$classes <- void_classes %>%
    filter(!is.na(class), nzchar(class)) %>%
    transmute(uri = class, label = local_name(class)) %>%
    distinct(uri, .keep_all = TRUE)

  schema$properties <- void_props %>%
    filter(!is.na(prop), nzchar(prop)) %>%
    transmute(property = prop, label = local_name(prop)) %>%
    distinct(property, .keep_all = TRUE)

  schema
}
