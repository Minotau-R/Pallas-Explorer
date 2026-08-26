test_that("extract_schema dispatches to the OWL extractor when owl:Class/rdfs:Class exist", {
  testthat::local_mocked_bindings(
    sparql_query = function(url, query, timeout = 60) data.frame(n = "3", stringsAsFactors = FALSE),
    extract_schema_owl = function(source) "OWL",
    extract_schema_void = function(source) stop("should not be called"),
    extract_schema_generic = function(source) stop("should not be called")
  )

  expect_equal(extract_schema("https://example.org/sparql"), "OWL")
})

test_that("extract_schema falls back to VoID when there is no OWL signal", {
  fake_sparql <- function(url, query, timeout = 60) {
    if (grepl("owl:Class", query, fixed = TRUE)) {
      data.frame(n = "0", stringsAsFactors = FALSE)
    } else {
      data.frame(class = "http://ex.org/Dataset", prop = NA_character_, stringsAsFactors = FALSE)
    }
  }
  testthat::local_mocked_bindings(
    sparql_query = fake_sparql,
    extract_schema_void = function(source) "VOID",
    extract_schema_generic = function(source) stop("should not be called")
  )

  expect_equal(extract_schema("https://example.org/sparql"), "VOID")
})

test_that("extract_schema falls back to generic RDF when there is no OWL or VoID signal", {
  testthat::local_mocked_bindings(
    sparql_query = function(url, query, timeout = 60) data.frame(),
    extract_schema_generic = function(source) "GENERIC"
  )

  expect_equal(extract_schema("https://example.org/sparql"), "GENERIC")
})

test_that("extract_schema falls back to generic RDF when detection queries fail", {
  testthat::local_mocked_bindings(
    sparql_query = function(url, query, timeout = 60) stop("endpoint down"),
    extract_schema_generic = function(source) "GENERIC"
  )

  expect_equal(suppressWarnings(extract_schema("https://example.org/sparql")), "GENERIC")
})
