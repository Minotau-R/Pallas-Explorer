test_that(".schema_uri_universe collects every referenced id, deduplicated", {
  schema <- .empty_schema()
  schema$classes <- data.frame(uri = "http://ex.org/A", label = "A", stringsAsFactors = FALSE)
  schema$subclass <- data.frame(child = "http://ex.org/A", parent = "http://ex.org/A",
                                stringsAsFactors = FALSE)
  schema$restrictions <- data.frame(
    restriction = "http://ex.org/r1", onProperty = "http://ex.org/p1",
    target = NA_character_, relation = "someValuesFrom", cardinality_label = NA_character_,
    stringsAsFactors = FALSE
  )

  ids <- .schema_uri_universe(schema)

  expect_setequal(ids, c("http://ex.org/A", "http://ex.org/r1", "http://ex.org/p1"))
})

test_that(".schema_uri_universe returns an empty vector for an empty schema", {
  expect_equal(.schema_uri_universe(.empty_schema()), character(0))
})

test_that(".classify_node classifies nodes with Restriction taking priority", {
  schema <- .empty_schema()
  schema$classes <- data.frame(uri = "http://ex.org/A", label = NA_character_, stringsAsFactors = FALSE)
  schema$properties <- data.frame(property = "http://ex.org/p", label = NA_character_,
                                  stringsAsFactors = FALSE)
  schema$property_ranges <- data.frame(property = "http://ex.org/p", range = "http://ex.org/Dt",
                                       stringsAsFactors = FALSE)
  schema$restrictions <- data.frame(
    restriction = "http://ex.org/A", onProperty = "http://ex.org/p",
    target = NA_character_, relation = "someValuesFrom", cardinality_label = NA_character_,
    stringsAsFactors = FALSE
  )

  types <- .classify_node(
    c("http://ex.org/A", "http://ex.org/p", "http://ex.org/Dt", "http://ex.org/unseen"), schema
  )

  expect_equal(types, c("Restriction", "Property", "Datatype", "Node"))
})

test_that(".classify_node treats everything as Node for an empty schema", {
  expect_equal(.classify_node("http://ex.org/X", .empty_schema()), "Node")
})


test_that(".restriction_label formats cardinality when present", {
  restrictions <- data.frame(
    restriction = c("r1", "r2"),
    relation = c("someValuesFrom", "cardinality"),
    cardinality_label = c(NA_character_, "exactly 1"),
    stringsAsFactors = FALSE
  )

  expect_equal(
    .restriction_label(restrictions),
    c("Restriction (someValuesFrom)", "Restriction (cardinality, exactly 1)")
  )
})

test_that(".restriction_label returns character(0) for no restrictions", {
  expect_equal(.restriction_label(.empty_schema()$restrictions), character(0))
})

test_that("build_schema_graph builds nodes and edges from a small schema", {
  schema <- .empty_schema()
  schema$classes <- data.frame(uri = c("http://ex.org/Person", "http://ex.org/Student"),
                               label = c("Person", NA_character_), stringsAsFactors = FALSE)
  schema$subclass <- data.frame(child = "http://ex.org/Student", parent = "http://ex.org/Person",
                                stringsAsFactors = FALSE)

  g <- build_schema_graph(schema)

  expect_s3_class(g, "igraph")
  expect_equal(igraph::vcount(g), 2)
  expect_equal(igraph::ecount(g), 1)
  expect_true("subClassOf" %in% igraph::E(g)$relation)
})

test_that("build_schema_graph warns and returns an empty graph for an empty schema", {
  expect_warning(g <- build_schema_graph(.empty_schema()), "empty")
  expect_equal(igraph::vcount(g), 0)
})

test_that("build_schema_graph tolerates a malformed/incomplete schema list", {
  expect_warning(g <- build_schema_graph(list()), "empty")
  expect_equal(igraph::vcount(g), 0)
})
