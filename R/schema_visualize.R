
.node_color_map <- c(
  Class = "#a3cef1", Property = "#f4a261", Restriction = "#c9a0dc",
  Datatype = "#f6e05e", Node = "#e2e8f0"
)

.edge_style_map <- list(
  subClassOf = list(color = "#2b6cb0", dashes = FALSE),
  domain = list(color = "#38a169", dashes = FALSE),
  range = list(color = "#d69e2e", dashes = FALSE),
  onProperty = list(color = "#805ad5", dashes = TRUE),
  someValuesFrom = list(color = "#805ad5", dashes = TRUE),
  allValuesFrom = list(color = "#805ad5", dashes = TRUE),
  hasValue = list(color = "#805ad5", dashes = TRUE),
  onClass = list(color = "#805ad5", dashes = TRUE),
  restriction = list(color = "#805ad5", dashes = TRUE),
  unionOf = list(color = "#dd6b20", dashes = TRUE),
  intersectionOf = list(color = "#dd6b20", dashes = TRUE),
  oneOf = list(color = "#dd6b20", dashes = TRUE),
  equivalentClass = list(color = "#319795", dashes = TRUE),
  equivalentProperty = list(color = "#319795", dashes = TRUE),
  inverseOf = list(color = "#3182ce", dashes = TRUE),
  subPropertyOf = list(color = "#2b6cb0", dashes = TRUE),
  disjointWith = list(color = "#e53e3e", dashes = TRUE),
  propertyDisjointWith = list(color = "#e53e3e", dashes = TRUE),
  complementOf = list(color = "#e53e3e", dashes = TRUE)
)
.default_edge_style <- list(color = "#718096", dashes = TRUE)

#' @noRd
.edge_color_for <- function(relation) {
  unname(vapply(relation, function(r) {
    style <- .edge_style_map[[r]]
    if (is.null(style)) .default_edge_style$color else style$color
  }, character(1)))
}

#' @noRd
.edge_dashes_for <- function(relation) {
  vapply(relation, function(r) {
    style <- .edge_style_map[[r]]
    if (is.null(style)) .default_edge_style$dashes else style$dashes
  }, logical(1))
}

#' Build the visNetwork node table for a graph
#'
#' Separated from [visualise_schema()] so the size-driven layout choice
#' (precomputed `x`/`y` vs. none) can be unit-tested as plain data, without
#' needing `visNetwork` or an actual rendered widget.
#'
#' @param g An `igraph` graph, see [build_schema_graph()].
#' @param use_static_layout If `TRUE`, add `x`/`y` columns from
#'   `igraph::layout_nicely()` so the caller can disable running physics.
#' @return A data frame with `id`, `label`, `title`, `color`, `group`, and
#'   (if requested) `x`/`y`.
#' @noRd
.schema_vis_nodes <- function(g, use_static_layout) {
  nodes <- data.frame(
    id    = igraph::V(g)$name,
    label = igraph::V(g)$label,
    title = igraph::V(g)$name,
    color = unname(.node_color_map[igraph::V(g)$type]),
    group = igraph::V(g)$type,
    stringsAsFactors = FALSE
  )
  if (isTRUE(use_static_layout)) {
    coords <- igraph::layout_nicely(g) * 300
    nodes$x <- coords[, 1]
    nodes$y <- coords[, 2]
  }
  nodes
}

#' Render a schema graph as an interactive visNetwork widget
#'
#' Refuses to render graphs above `max_nodes` by default (an interactive
#' force-directed layout with thousands of nodes is unreadable and can
#' freeze a browser tab). Graphs above `physics_threshold` get a
#' precomputed static layout and straight edges instead of a running
#' physics simulation, which is what actually makes large graphs feel
#' sluggish in-browser.
#'
#' @param g An `igraph` graph, see [build_schema_graph()].
#' @param title Character scalar shown as the widget's title.
#' @param max_nodes Refuse to render above this many nodes unless
#'   `force = TRUE`. Default `750`.
#' @param physics_threshold Above this many nodes, use a precomputed
#'   static layout and disable the running physics simulation. Default
#'   `150`.
#' @param force If `TRUE`, render anyway above `max_nodes` (with a
#'   warning). Default `FALSE`.
#' @return A `visNetwork` htmlwidget.
#' @export
visualise_schema <- function(g, title = "Schema Graph",
                             max_nodes = 750,
                             physics_threshold = 150,
                             force = FALSE) {
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop(
      "Rendering a schema graph requires the 'visNetwork' package. ",
      "Install it with install.packages('visNetwork').",
      call. = FALSE
    )
  }

  n <- igraph::vcount(g)

  if (n == 0) {
    warning("Graph has no nodes. Returning empty widget.", call. = FALSE)
    return(visNetwork::visNetwork(
      data.frame(id = integer(), label = character()),
      data.frame(from = integer(), to = integer())
    ))
  }

  if (n > max_nodes) {
    if (!isTRUE(force)) {
      stop(
        "Graph has ", n, " nodes, which exceeds max_nodes = ", max_nodes, ". ",
        "Rendering this many nodes as an interactive widget is likely to be ",
        "slow and hard to read. Either reduce the schema before calling ",
        "build_schema_graph(), raise max_nodes, or pass force = TRUE to ",
        "render anyway.",
        call. = FALSE
      )
    }
    warning(
      "Rendering ", n, " nodes despite exceeding max_nodes = ", max_nodes,
      " (force = TRUE). This may be slow and hard to read.",
      call. = FALSE
    )
  }

  use_static_layout <- n > physics_threshold
  vis_nodes <- .schema_vis_nodes(g, use_static_layout)

  if (igraph::ecount(g) == 0) {
    vis_edges <- data.frame(from = character(), to = character(), title = character(),
                            arrows = character(), color = character(), dashes = logical(),
                            stringsAsFactors = FALSE)
  } else {
    rel <- igraph::E(g)$relation
    ends_mat <- igraph::ends(g, igraph::E(g))
    vis_edges <- data.frame(
      from   = ends_mat[, 1],
      to     = ends_mat[, 2],
      label  = rel,
      title  = rel,
      arrows = "to",
      color  = .edge_color_for(rel),
      dashes = .edge_dashes_for(rel),
      stringsAsFactors = FALSE
    )
  }

  vis <- visNetwork::visNetwork(vis_nodes, vis_edges, main = title) |>
    visNetwork::visEdges(smooth = !use_static_layout) |>
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = 2),
                           nodesIdSelection = TRUE) |>
    visNetwork::visLegend()

  if (use_static_layout) {
    vis |> visNetwork::visPhysics(enabled = FALSE)
  } else {
    vis |> visNetwork::visPhysics(solver = "barnesHut")
  }
}
