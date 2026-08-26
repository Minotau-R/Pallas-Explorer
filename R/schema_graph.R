#' @importFrom dplyr %>% filter transmute select bind_rows distinct coalesce
NULL

#' Full closure of every URI/blank node referenced by a schema
#' @param schema A schema list, see [extract_schema()].
#' @return A character vector of unique, non-missing node ids.
#' @noRd
.schema_uri_universe <- function(schema) {
  ids <- c(
    schema$classes$uri,
    schema$properties$property,
    schema$subclass$child, schema$subclass$parent,
    schema$property_domains$property, schema$property_domains$domain,
    schema$property_ranges$property, schema$property_ranges$range,
    schema$extra_edges$from, schema$extra_edges$to,
    schema$union_intersection$from, schema$union_intersection$member,
    schema$restrictions$restriction, schema$restrictions$onProperty,
    schema$restrictions$target
  )
  ids <- unique(ids)
  ids[!is.na(ids) & nzchar(ids)]
}

#' Classify nodes as Class / Property / Restriction / Datatype / Node
#'
#' Priority (most specific wins): Restriction > Property > Class > Datatype
#' (only ever seen as a property range) > Node (individuals, cross-ontology
#' links, ...).
#'
#' @param uris Character vector of node ids.
#' @param schema A schema list, see [extract_schema()].
#' @return A character vector the same length as `uris`.
#' @noRd
.classify_node <- function(uris, schema) {
  type <- rep("Node", length(uris))
  type[uris %in% schema$property_ranges$range] <- "Datatype"
  type[uris %in% schema$classes$uri] <- "Class"
  type[uris %in% schema$properties$property] <- "Property"
  type[uris %in% schema$restrictions$restriction] <- "Restriction"
  type
}

#' Human-readable labels for owl:Restriction nodes
#' @param restrictions The `restrictions` data frame of a schema list.
#' @return A character vector the same length as `nrow(restrictions)`.
#' @noRd
.restriction_label <- function(restrictions) {
  if (nrow(restrictions) == 0) return(character(0))
  ifelse(
    !is.na(restrictions$cardinality_label),
    paste0("Restriction (", restrictions$relation, ", ", restrictions$cardinality_label, ")"),
    paste0("Restriction (", restrictions$relation, ")")
  )
}

#' Best available display label for each node
#' @param uris Character vector of node ids.
#' @param schema A schema list, see [extract_schema()].
#' @return A character vector the same length as `uris`.
#' @noRd
.pick_label <- function(uris, schema) {
  restr_lbl <- .restriction_label(schema$restrictions)
  coalesce(
    schema$classes$label[match(uris, schema$classes$uri)],
    schema$properties$label[match(uris, schema$properties$property)],
    restr_lbl[match(uris, schema$restrictions$restriction)],
    local_name(uris)
  )
}

#' Build an igraph schema graph from an extracted schema
#'
#' @param schema A schema list, see [extract_schema()].
#' @return A directed `igraph` graph. Returns an empty graph (with a
#'   warning) when the schema has no classes/properties/axioms.
#' @export
build_schema_graph <- function(schema) {
  universe <- .schema_uri_universe(schema)
  if (length(universe) == 0) {
    warning("Schema is empty.", call. = FALSE)
    return(igraph::make_empty_graph(directed = TRUE))
  }

  nodes <- data.frame(name = universe, stringsAsFactors = FALSE)
  nodes$label <- .pick_label(nodes$name, schema)
  nodes$type  <- .classify_node(nodes$name, schema)

  edges_sub    <- transmute(schema$subclass, from = child, to = parent, relation = "subClassOf")
  edges_domain <- transmute(schema$property_domains, from = property, to = domain, relation = "domain")
  edges_range  <- transmute(schema$property_ranges, from = property, to = range, relation = "range")
  edges_extra  <- select(schema$extra_edges, from, to, relation)
  edges_ui     <- transmute(schema$union_intersection, from = from, to = member, relation = relation)

  edges_restr_prop <- schema$restrictions %>%
    filter(!is.na(onProperty)) %>%
    transmute(from = restriction, to = onProperty, relation = "onProperty")

  edges_restr_target <- schema$restrictions %>%
    filter(!is.na(target)) %>%
    transmute(from = restriction, to = target, relation = relation)

  edges_all <- bind_rows(edges_sub, edges_domain, edges_range, edges_extra,
                         edges_ui, edges_restr_prop, edges_restr_target) %>%
    filter(!is.na(from), !is.na(to), nzchar(from), nzchar(to)) %>%
    distinct(from, to, relation)

  igraph::graph_from_data_frame(edges_all, vertices = nodes, directed = TRUE)
}
