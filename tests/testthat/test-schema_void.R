test_that("extract_schema_void reads void:class/property declarations", {
  fake_sparql <- function(url, query, timeout = 60) {
    if (grepl("void:class", query, fixed = TRUE)) {
      data.frame(class = "http://ex.org/Dataset", stringsAsFactors = FALSE)
    } else {
      data.frame(prop = "http://purl.org/dc/terms/title", stringsAsFactors = FALSE)
    }
  }
  testthat::local_mocked_bindings(sparql_query = fake_sparql)

  schema <- extract_schema_void("https://example.org/sparql")

  expect_equal(schema$classes$uri, "http://ex.org/Dataset")
  expect_equal(schema$classes$label, "Dataset")
  expect_equal(schema$properties$property, "http://purl.org/dc/terms/title")
  expect_equal(schema$properties$label, "title")
})

test_that("extract_schema_void returns empty tables when there is no VoID data", {
  testthat::local_mocked_bindings(
    sparql_query = function(url, query, timeout = 60) data.frame()
  )

  schema <- extract_schema_void("https://example.org/sparql")

  expect_equal(nrow(schema$classes), 0)
  expect_equal(names(schema$classes), c("uri", "label"))
  expect_equal(nrow(schema$properties), 0)
})

test_that("extract_schema_void tolerates a failing endpoint", {
  testthat::local_mocked_bindings(
    sparql_query = function(url, query, timeout = 60) stop("timeout")
  )

  schema <- suppressWarnings(extract_schema_void("https://example.org/sparql"))

  expect_equal(nrow(schema$classes), 0)
  expect_equal(nrow(schema$properties), 0)
})
