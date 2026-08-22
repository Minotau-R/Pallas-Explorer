test_that("extract_list_edges fully traverses a multi-member rdf:List", {
  fake_sparql <- function(url, query, timeout = 60) {
    if (grepl("unionOf", query, fixed = TRUE)) {
      data.frame(class = "http://ex.org/A", list = "http://ex.org/list1",
                 relation = "unionOf", stringsAsFactors = FALSE)
    } else if (grepl("rdf:first", query, fixed = TRUE)) {
      data.frame(
        list  = c("http://ex.org/list1", "http://ex.org/list2"),
        first = c("http://ex.org/X", "http://ex.org/Y"),
        rest  = c("http://ex.org/list2", RDF_NIL),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame()
    }
  }
  testthat::local_mocked_bindings(sparql_query = fake_sparql)

  result <- extract_list_edges("https://example.org/sparql")

  expect_equal(nrow(result), 2)
  expect_setequal(result$member, c("http://ex.org/X", "http://ex.org/Y"))
  expect_true(all(result$relation == "unionOf"))
})

test_that("extract_list_edges guards against a cyclic list without hanging", {
  fake_sparql <- function(url, query, timeout = 60) {
    if (grepl("unionOf", query, fixed = TRUE)) {
      data.frame(class = "http://ex.org/A", list = "http://ex.org/loop",
                 relation = "unionOf", stringsAsFactors = FALSE)
    } else if (grepl("rdf:first", query, fixed = TRUE)) {
      data.frame(list = "http://ex.org/loop", first = "http://ex.org/X",
                 rest = "http://ex.org/loop", stringsAsFactors = FALSE)
    } else {
      data.frame()
    }
  }
  testthat::local_mocked_bindings(sparql_query = fake_sparql)

  result <- extract_list_edges("https://example.org/sparql")

  expect_equal(nrow(result), 1)
  expect_equal(result$member, "http://ex.org/X")
})

test_that("extract_list_edges tolerates a failing endpoint", {
  testthat::local_mocked_bindings(
    sparql_query = function(url, query, timeout = 60) stop("network down")
  )

  expect_warning(result <- extract_list_edges("https://example.org/sparql"), "network down")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("from", "member", "relation"))
})

test_that("extract_restrictions derives a cardinality_label per restriction", {
  fake_sparql <- function(url, query, timeout = 60) {
    data.frame(
      restriction = c("http://ex.org/r1", "http://ex.org/r2"),
      onProperty  = c("http://ex.org/hasChild", "http://ex.org/hasSpouse"),
      target      = c("http://ex.org/Person", NA),
      relation    = c("someValuesFrom", "hasValue"),
      cardinality = c(NA, "1"),
      minCardinality = NA_character_, maxCardinality = NA_character_,
      qualifiedCardinality = NA_character_, minQualifiedCardinality = NA_character_,
      maxQualifiedCardinality = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  testthat::local_mocked_bindings(sparql_query = fake_sparql)

  result <- extract_restrictions("https://example.org/sparql")

  expect_equal(nrow(result), 2)
  expect_true(is.na(result$cardinality_label[result$restriction == "http://ex.org/r1"]))
  expect_equal(result$cardinality_label[result$restriction == "http://ex.org/r2"], "exactly 1")
})

test_that("extract_restrictions returns an empty, well-shaped result when there are none", {
  testthat::local_mocked_bindings(
    sparql_query = function(url, query, timeout = 60) data.frame()
  )

  result <- extract_restrictions("https://example.org/sparql")

  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("restriction", "onProperty", "target", "relation", "cardinality_label"))
})

test_that("extract_restrictions tolerates a failing endpoint", {
  testthat::local_mocked_bindings(
    sparql_query = function(url, query, timeout = 60) stop("boom")
  )

  expect_warning(result <- extract_restrictions("https://example.org/sparql"), "boom")
  expect_equal(nrow(result), 0)
})

test_that("extract_schema_owl assembles classes, properties and axioms", {
  fake_sparql <- function(url, query, timeout = 60) {
    if (grepl("a owl:Class", query, fixed = TRUE)) {
      data.frame(uri = "http://ex.org/Person", label = "Person", stringsAsFactors = FALSE)
    } else if (grepl("rdfs:subClassOf ?parent", query, fixed = TRUE)) {
      data.frame(child = "http://ex.org/Student", parent = "http://ex.org/Person",
                 stringsAsFactors = FALSE)
    } else if (grepl("?ptype IN", query, fixed = TRUE)) {
      data.frame(property = "http://ex.org/name", label = "name", stringsAsFactors = FALSE)
    } else if (grepl("rdfs:domain ?domain", query, fixed = TRUE)) {
      data.frame(property = "http://ex.org/name", domain = "http://ex.org/Person",
                 stringsAsFactors = FALSE)
    } else if (grepl("rdfs:range ?range", query, fixed = TRUE)) {
      data.frame(property = "http://ex.org/name",
                 range = "http://www.w3.org/2001/XMLSchema#string", stringsAsFactors = FALSE)
    } else if (grepl("owl:equivalentClass", query, fixed = TRUE)) {
      data.frame(from = "http://ex.org/Person", to = "http://ex.org/Human",
                 relation = "equivalentClass", stringsAsFactors = FALSE)
    } else {
      data.frame()
    }
  }
  testthat::local_mocked_bindings(
    sparql_query = fake_sparql,
    extract_list_edges = function(source) {
      data.frame(from = character(), member = character(), relation = character(),
                 stringsAsFactors = FALSE)
    },
    extract_restrictions = function(source) {
      data.frame(restriction = character(), onProperty = character(), target = character(),
                 relation = character(), cardinality_label = character(), stringsAsFactors = FALSE)
    }
  )

  schema <- extract_schema_owl("https://example.org/sparql")

  expect_equal(schema$classes$uri, "http://ex.org/Person")
  expect_equal(schema$subclass$child, "http://ex.org/Student")
  expect_equal(schema$properties$property, "http://ex.org/name")
  expect_equal(schema$property_domains$domain, "http://ex.org/Person")
  expect_equal(schema$property_ranges$range, "http://www.w3.org/2001/XMLSchema#string")
  expect_equal(schema$extra_edges$relation, "equivalentClass")
})

test_that("extract_schema_owl returns an empty-but-well-shaped schema for an OWL-free source", {
  testthat::local_mocked_bindings(
    sparql_query = function(url, query, timeout = 60) data.frame(),
    extract_list_edges = function(source) {
      data.frame(from = character(), member = character(), relation = character(),
                 stringsAsFactors = FALSE)
    },
    extract_restrictions = function(source) {
      data.frame(restriction = character(), onProperty = character(), target = character(),
                 relation = character(), cardinality_label = character(), stringsAsFactors = FALSE)
    }
  )

  schema <- extract_schema_owl("https://example.org/sparql")

  expect_equal(nrow(schema$classes), 0)
  expect_equal(nrow(schema$properties), 0)
  expect_equal(nrow(schema$subclass), 0)
})

test_that("extract_schema_owl degrades gracefully when every query fails", {
  testthat::local_mocked_bindings(
    sparql_query = function(url, query, timeout = 60) stop("endpoint unreachable")
  )

  schema <- suppressWarnings(extract_schema_owl("https://example.org/sparql"))

  expect_equal(nrow(schema$classes), 0)
  expect_equal(nrow(schema$restrictions), 0)
  expect_equal(names(schema$classes), c("uri", "label"))
})
