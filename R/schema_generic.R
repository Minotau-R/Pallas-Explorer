#' @importFrom dplyr %>% filter transmute distinct
NULL


#' Extract a schema from generic RDF data.
#'
#' @param source A SPARQL endpoint URL or a parsed `rdflib` compatible file.
#' @return A schema list, see `.empty_schema()`.
#' @export
extract_schema_generic <- function(source) {
  schema <- .empty_schema()

  data_classes <- safe_query(source, '
    SELECT DISTINCT ?class WHERE { ?s a ?class . FILTER(isIRI(?class)) }',
                             expected_cols = "class")

  data_props <- safe_query(source, '
    SELECT DISTINCT ?prop WHERE { ?s ?prop ?o . FILTER(isIRI(?prop)) }',
                           expected_cols = "prop")

  class_labels <- safe_query(source, '
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?class ?label WHERE {
      ?class rdfs:label ?label .
      BIND(IF(LANG(?label) = "en" || LANG(?label) = "", 0, 1) AS ?labelRank)
    } ORDER BY ?class ?labelRank', expected_cols = c("class", "label"))

  prop_labels <- safe_query(source, '
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?prop ?label WHERE {
      ?prop rdfs:label ?label .
      BIND(IF(LANG(?label) = "en" || LANG(?label) = "", 0, 1) AS ?labelRank)
    } ORDER BY ?prop ?labelRank', expected_cols = c("prop", "label"))

  domains <- safe_query(source, '
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT DISTINCT ?prop ?domain WHERE { ?prop rdfs:domain ?domain . }',
                        expected_cols = c("prop", "domain"))

  ranges <- safe_query(source, '
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT DISTINCT ?prop ?range WHERE { ?prop rdfs:range ?range . }',
                       expected_cols = c("prop", "range"))

  class_label_lookup <- class_labels %>% distinct(class, .keep_all = TRUE)
  prop_label_lookup  <- prop_labels  %>% distinct(prop, .keep_all = TRUE)

  schema$classes <- data_classes %>%
    filter(!is.na(class), nzchar(class)) %>%
    transmute(uri = class,
              label = class_label_lookup$label[match(class, class_label_lookup$class)]) %>%
    distinct(uri, .keep_all = TRUE)

  schema$properties <- data_props %>%
    filter(!is.na(prop), nzchar(prop)) %>%
    transmute(property = prop,
              label = prop_label_lookup$label[match(prop, prop_label_lookup$prop)]) %>%
    distinct(property, .keep_all = TRUE)

  schema$property_domains <- domains %>%
    filter(!is.na(domain), nzchar(domain)) %>%
    transmute(property = prop, domain = domain)

  schema$property_ranges <- ranges %>%
    filter(!is.na(range), nzchar(range)) %>%
    transmute(property = prop, range = range)

  schema
}
