#' @importFrom dplyr %>% filter mutate distinct bind_rows select case_when
NULL

RDF_NIL <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"

#' Fully traverse every rdf:List referenced by owl:unionOf/intersectionOf/oneOf
#'
#' Queries all rdf:first/rdf:rest cons-cells up front and walks each list to
#' its end, so lists with more than one member are captured completely.
#'
#' @param source A SPARQL endpoint URL or a parsed rdflib compatible file.
#' @return A data frame with columns `from`, `member`, `relation`.
#' @export
extract_list_edges <- function(source) {
  heads <- safe_query(source, '
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    SELECT ?class ?list ?relation WHERE {
      { ?class owl:unionOf ?list        . BIND("unionOf" AS ?relation) }
      UNION { ?class owl:intersectionOf ?list . BIND("intersectionOf" AS ?relation) }
      UNION { ?class owl:oneOf ?list          . BIND("oneOf" AS ?relation) }
    }', expected_cols = c("class", "list", "relation"))

  if (nrow(heads) == 0) {
    return(data.frame(from = character(), member = character(), relation = character(),
                      stringsAsFactors = FALSE))
  }

  cells <- safe_query(source, '
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?list ?first ?rest WHERE {
      ?list rdf:first ?first .
      OPTIONAL { ?list rdf:rest ?rest }
    }', expected_cols = c("list", "first", "rest"))

  first_of <- cells %>% filter(!is.na(first)) %>% distinct(list, first)
  rest_of  <- cells %>% filter(!is.na(rest))  %>% distinct(list, rest)

  out <- vector("list", nrow(heads))
  for (i in seq_len(nrow(heads))) {
    class_iri <- heads$class[i]
    rel       <- heads$relation[i]
    current   <- heads$list[i]
    visited   <- character()
    members   <- character()
    while (!is.na(current) && current != RDF_NIL && !(current %in% visited)) {
      visited <- c(visited, current)
      first_val <- first_of$first[match(current, first_of$list)]
      if (!is.na(first_val)) members <- c(members, first_val)
      current <- rest_of$rest[match(current, rest_of$list)]
    }
    if (length(members) > 0) {
      out[[i]] <- data.frame(from = class_iri, member = members, relation = rel,
                             stringsAsFactors = FALSE)
    }
  }

  result <- bind_rows(out)
  if (nrow(result) == 0) {
    return(data.frame(from = character(), member = character(), relation = character(),
                      stringsAsFactors = FALSE))
  }
  distinct(result)
}

#' Extract owl:Restriction axioms
#'
#' Captures the restricted property (`owl:onProperty`), the restriction kind
#' (someValuesFrom/allValuesFrom/hasValue/onClass), and a human-readable
#' cardinality label when a cardinality constraint is present.
#'
#' @param source A SPARQL endpoint URL or a parsed rdflib compatible file.
#' @return A data frame with columns `restriction`, `onProperty`, `target`,
#'   `relation`, `cardinality_label`.
#' @export
extract_restrictions <- function(source) {
  cols <- c("restriction", "onProperty", "target", "relation", "cardinality",
            "minCardinality", "maxCardinality", "qualifiedCardinality",
            "minQualifiedCardinality", "maxQualifiedCardinality")

  raw <- safe_query(source, '
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    SELECT ?restriction ?onProperty ?target ?relation
           ?cardinality ?minCardinality ?maxCardinality
           ?qualifiedCardinality ?minQualifiedCardinality ?maxQualifiedCardinality
    WHERE {
      ?restriction a owl:Restriction ;
                   owl:onProperty ?onProperty .
      OPTIONAL { ?restriction owl:someValuesFrom ?someValuesFrom }
      OPTIONAL { ?restriction owl:allValuesFrom  ?allValuesFrom }
      OPTIONAL { ?restriction owl:hasValue       ?hasValue }
      OPTIONAL { ?restriction owl:onClass        ?onClass }
      OPTIONAL { ?restriction owl:cardinality ?cardinality }
      OPTIONAL { ?restriction owl:minCardinality ?minCardinality }
      OPTIONAL { ?restriction owl:maxCardinality ?maxCardinality }
      OPTIONAL { ?restriction owl:qualifiedCardinality ?qualifiedCardinality }
      OPTIONAL { ?restriction owl:minQualifiedCardinality ?minQualifiedCardinality }
      OPTIONAL { ?restriction owl:maxQualifiedCardinality ?maxQualifiedCardinality }
      BIND(COALESCE(?someValuesFrom, ?allValuesFrom, ?hasValue, ?onClass) AS ?target)
      BIND(
        IF(BOUND(?someValuesFrom), "someValuesFrom",
        IF(BOUND(?allValuesFrom), "allValuesFrom",
        IF(BOUND(?hasValue), "hasValue",
        IF(BOUND(?onClass), "onClass", "restriction")))) AS ?relation)
    }', expected_cols = cols)

  if (nrow(raw) == 0) {
    return(data.frame(restriction = character(), onProperty = character(),
                      target = character(), relation = character(),
                      cardinality_label = character(), stringsAsFactors = FALSE))
  }

  raw %>%
    mutate(cardinality_label = case_when(
      !is.na(cardinality) ~ paste0("exactly ", cardinality),
      !is.na(minCardinality) & !is.na(maxCardinality) ~ paste0(minCardinality, "..", maxCardinality),
      !is.na(minCardinality) ~ paste0("min ", minCardinality),
      !is.na(maxCardinality) ~ paste0("max ", maxCardinality),
      !is.na(qualifiedCardinality) ~ paste0("exactly ", qualifiedCardinality, " (qualified)"),
      !is.na(minQualifiedCardinality) ~ paste0("min ", minQualifiedCardinality, " (qualified)"),
      !is.na(maxQualifiedCardinality) ~ paste0("max ", maxQualifiedCardinality, " (qualified)"),
      TRUE ~ NA_character_
    )) %>%
    select(restriction, onProperty, target, relation, cardinality_label) %>%
    distinct()
}

#' Extract a schema from an OWL ontology
#'
#' @param source A SPARQL endpoint URL or a parsed rdflib compatible file.
#' @return A schema list, see [.empty_schema()].
#' @export
extract_schema_owl <- function(source) {
  schema <- .empty_schema()

  classes <- safe_query(source, '
    PREFIX owl:  <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?uri ?label WHERE {
      { ?uri a owl:Class } UNION { ?uri a rdfs:Class }
      OPTIONAL {
        ?uri rdfs:label ?label .
        BIND(IF(LANG(?label) = "en" || LANG(?label) = "", 0, 1) AS ?labelRank)
      }
    } ORDER BY ?uri ?labelRank', expected_cols = c("uri", "label"))

  subclass <- safe_query(source, '
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?child ?parent WHERE { ?child rdfs:subClassOf ?parent . }',
                         expected_cols = c("child", "parent"))

  properties <- safe_query(source, '
    PREFIX owl:  <http://www.w3.org/2002/07/owl#>
    PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?property ?label WHERE {
      ?property a ?ptype .
      FILTER(?ptype IN (owl:ObjectProperty, owl:DatatypeProperty,
                        owl:AnnotationProperty, rdf:Property))
      OPTIONAL {
        ?property rdfs:label ?label .
        BIND(IF(LANG(?label) = "en" || LANG(?label) = "", 0, 1) AS ?labelRank)
      }
    } ORDER BY ?property ?labelRank', expected_cols = c("property", "label"))

  property_domains <- safe_query(source, '
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT DISTINCT ?property ?domain WHERE { ?property rdfs:domain ?domain . }',
                                 expected_cols = c("property", "domain"))

  property_ranges <- safe_query(source, '
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT DISTINCT ?property ?range WHERE { ?property rdfs:range ?range . }',
                                expected_cols = c("property", "range"))

  extra_edges <- safe_query(source, '
    PREFIX owl:  <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?from ?to ?relation WHERE {
      { ?from rdfs:subPropertyOf ?to           . BIND("subPropertyOf" AS ?relation) }
      UNION { ?from owl:equivalentClass ?to        . BIND("equivalentClass" AS ?relation) }
      UNION { ?from owl:equivalentProperty ?to     . BIND("equivalentProperty" AS ?relation) }
      UNION { ?from owl:inverseOf ?to              . BIND("inverseOf" AS ?relation) }
      UNION { ?from owl:disjointWith ?to           . BIND("disjointWith" AS ?relation) }
      UNION { ?from owl:propertyDisjointWith ?to   . BIND("propertyDisjointWith" AS ?relation) }
      UNION { ?from owl:complementOf ?to           . BIND("complementOf" AS ?relation) }
    }', expected_cols = c("from", "to", "relation"))

  schema$classes            <- classes %>% distinct(uri, .keep_all = TRUE)
  schema$subclass           <- subclass %>% filter(!is.na(child), !is.na(parent)) %>% distinct()
  schema$properties         <- properties %>% distinct(property, .keep_all = TRUE)
  schema$property_domains   <- property_domains %>% filter(!is.na(domain)) %>% distinct()
  schema$property_ranges    <- property_ranges %>% filter(!is.na(range)) %>% distinct()
  schema$extra_edges        <- extra_edges %>% filter(!is.na(from), !is.na(to)) %>% distinct()
  schema$union_intersection <- extract_list_edges(source)
  schema$restrictions       <- extract_restrictions(source)

  schema
}
