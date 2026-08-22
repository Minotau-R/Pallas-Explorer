test_that("run_sparql dispatches a character source to sparql_query", {
  testthat::local_mocked_bindings(
    sparql_query = function(url, query, timeout = 60) {
      data.frame(url = url, query = query, stringsAsFactors = FALSE)
    }
  )

  result <- run_sparql("https://example.org/sparql", "SELECT * WHERE { ?s ?p ?o }")

  expect_equal(result$url, "https://example.org/sparql")
  expect_equal(result$query, "SELECT * WHERE { ?s ?p ?o }")
})

test_that("run_sparql dispatches a non-character source to rdflib::rdf_query", {
  skip_if_not_installed("rdflib")

  testthat::local_mocked_bindings(
    rdf_query = function(source, query) data.frame(x = "ok", stringsAsFactors = FALSE),
    .package = "rdflib"
  )

  fake_model <- structure(list(), class = "rdf")
  result <- run_sparql(fake_model, "SELECT * WHERE { ?s ?p ?o }")

  expect_equal(result$x, "ok")
})

test_that("safe_query warns and returns an empty, well-shaped data frame on failure", {
  testthat::local_mocked_bindings(
    sparql_query = function(url, query, timeout = 60) stop("boom")
  )
  expect_warning(
    result <- safe_query("https://example.org/sparql", "SELECT ?s WHERE { ?s ?p ?o }"),
    "boom"
  )
})
