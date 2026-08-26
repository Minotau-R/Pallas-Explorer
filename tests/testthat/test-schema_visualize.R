.make_test_graph <- function(n_nodes) {
  ids <- paste0("http://ex.org/N", seq_len(n_nodes))
  edges <- if (n_nodes > 1) {
    data.frame(from = ids[-n_nodes], to = ids[-1], relation = "subClassOf",
               stringsAsFactors = FALSE)
  } else {
    data.frame(from = character(), to = character(), relation = character(),
               stringsAsFactors = FALSE)
  }
  nodes <- data.frame(name = ids, label = ids, type = rep("Class", n_nodes),
                      stringsAsFactors = FALSE)
  igraph::graph_from_data_frame(edges, vertices = nodes, directed = TRUE)
}

test_that(".edge_color_for returns the mapped color for a known relation", {
  expect_equal(.edge_color_for("subClassOf"), "#2b6cb0")
})

test_that(".edge_color_for falls back to the default color for an unknown relation", {
  expect_equal(.edge_color_for("somethingMadeUp"), "#718096")
})

test_that(".edge_dashes_for reflects the style table, defaulting to dashed", {
  expect_false(.edge_dashes_for("domain"))
  expect_true(.edge_dashes_for("somethingMadeUp"))
})

test_that(".schema_vis_nodes adds a static layout only when requested", {
  g <- .make_test_graph(5)

  with_layout <- .schema_vis_nodes(g, use_static_layout = TRUE)
  without_layout <- .schema_vis_nodes(g, use_static_layout = FALSE)

  expect_true(all(c("x", "y") %in% names(with_layout)))
  expect_true(is.numeric(with_layout$x))
  expect_false(any(c("x", "y") %in% names(without_layout)))
})

test_that("visualise_schema renders a small graph as a visNetwork widget", {
  skip_if_not_installed("visNetwork")
  g <- .make_test_graph(3)

  vis <- visualise_schema(g, title = "Test")

  expect_s3_class(vis, "visNetwork")
})

test_that("visualise_schema warns and returns an empty widget for an empty graph", {
  skip_if_not_installed("visNetwork")
  g <- igraph::make_empty_graph(directed = TRUE)

  expect_warning(vis <- visualise_schema(g), "no nodes")
  expect_s3_class(vis, "visNetwork")
})

test_that("visualise_schema refuses to render above max_nodes by default", {
  skip_if_not_installed("visNetwork")
  g <- .make_test_graph(10)

  expect_error(visualise_schema(g, max_nodes = 5), "max_nodes")
})

test_that("visualise_schema renders anyway with force = TRUE, but warns", {
  skip_if_not_installed("visNetwork")
  g <- .make_test_graph(10)

  expect_warning(vis <- visualise_schema(g, max_nodes = 5, force = TRUE), "force")
  expect_s3_class(vis, "visNetwork")
})

test_that("visualise_schema uses a static layout above physics_threshold without erroring", {
  skip_if_not_installed("visNetwork")
  g <- .make_test_graph(20)

  vis <- visualise_schema(g, physics_threshold = 10, max_nodes = 100)

  expect_s3_class(vis, "visNetwork")
})

test_that("visualise_schema errors on a non-graph input", {
  skip_if_not_installed("visNetwork")
  expect_error(visualise_schema(list(not = "a graph")))
})
