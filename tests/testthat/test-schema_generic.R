test_that("extract_schema_generic infers classes/properties with labels and domains/ranges", {
  fake_sparql <- function(url, query, timeout = 60) {
    if (grepl("?s a ?class", query, fixed = TRUE)) {
      data.frame(class = "http://ex.org/Book", stringsAsFactors = FALSE)
    } else if (grepl("?s ?prop ?o", query, fixed = TRUE)) {
      data.frame(prop = "http://ex.org/title", stringsAsFactors = FALSE)
    } else if (grepl("?class rdfs:label", query, fixed = TRUE)) {
      data.frame(class = "http://ex.org/Book", label = "Book", stringsAsFactors = FALSE)
    } else if (grepl("?prop rdfs:label", query, fixed = TRUE)) {
      data.frame(prop = "http://ex.org/title", label = "Title", stringsAsFactors = FALSE)
    } else if (grepl("rdfs:domain", query, fixed = TRUE)) {
      data.frame(prop = "http://ex.org/title", domain = "http://ex.org/Book",
                 stringsAsFactors = FALSE)
    } else if (grepl("rdfs:range", query, fixed = TRUE)) {
      data.frame(prop = "http://ex.org/title",
                 range = "http://www.w3.org/2001/XMLSchema#string", stringsAsFactors = FALSE)
    } else {
      data.frame()
    }
  }
  testthat::local_mocked_bindings(sparql_query = fake_sparql)

  schema <- extract_schema_generic("https://example.org/sparql")

  expect_equal(schema$classes$label, "Book")
  expect_equal(schema$properties$label, "Title")
  expect_equal(schema$property_domains$domain, "http://ex.org/Book")
  expect_equal(schema$property_ranges$range, "http://www.w3.org/2001/XMLSchema#string")
})

test_that("extract_schema_generic falls back to no label when none is declared", {
  fake_sparql <- function(url, query, timeout = 60) {
    if (grepl("?s a ?class", query, fixed = TRUE)) {
      data.frame(class = "http://ex.org/Book", stringsAsFactors = FALSE)
    } else {
      data.frame()
    }
  }
  testthat::local_mocked_bindings(sparql_query = fake_sparql)

  schema <- extract_schema_generic("https://example.org/sparql")

  expect_equal(schema$classes$uri, "http://ex.org/Book")
  expect_true(is.na(schema$classes$label))
})

test_that("extract_schema_generic tolerates a failing endpoint", {
  testthat::local_mocked_bindings(
    sparql_query = function(url, query, timeout = 60) stop("boom")
  )

  schema <- suppressWarnings(extract_schema_generic("https://example.org/sparql"))

  expect_equal(nrow(schema$classes), 0)
  expect_equal(nrow(schema$properties), 0)
})
