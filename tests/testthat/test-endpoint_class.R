test_that("sparql_endpoint stores valid custom property values", {
  example_url <- "https://example.org/sparql"
  example_name <- "example"
  example_headers <- list(Accept = "application/json")
  example_timeout <- 30

  ep <- sparql_endpoint(
    url = example_url,
    name = example_name,
    headers = example_headers,
    timeout = example_timeout
  )

  expect_equal(ep@url, example_url)
  expect_equal(ep@name, example_name)
  expect_equal(ep@headers, example_headers)
  expect_equal(ep@timeout, example_timeout)
})

test_that("sparql_endpoint has the right defaults", {
  ep <- sparql_endpoint(url = "https://example.org/sparql")

  expect_true(is.na(ep@name))
  expect_equal(ep@headers, list())
  expect_equal(ep@timeout, 60)
})

test_that("sparql_endpoint@url validator rejects bad values", {
  expect_error(sparql_endpoint(url = ""), regexp = "non-empty")
  expect_error(sparql_endpoint(url = c("a", "b")), regexp = "single")
})

test_that("sparql_endpoint@name validator rejects non-scalar values", {
  expect_error(
    sparql_endpoint(url = "https://example.org", name = c("a", "b")),
    regexp = "single"
  )
})

test_that("sparql_endpoint@headers validator rejects unnamed lists", {
  expect_error(
    sparql_endpoint(url = "https://example.org", headers = list("application/json")),
    regexp = "named"
  )
  expect_error(
    sparql_endpoint(url = "https://example.org", headers = list(Accept = "a", "b")),
    regexp = "named"
  )
})

test_that("sparql_endpoint@timeout validator rejects non-positive or NA values", {
  expect_error(sparql_endpoint(url = "https://example.org", timeout = 0), regexp = "positive")
  expect_error(sparql_endpoint(url = "https://example.org", timeout = -5), regexp = "positive")
  expect_error(sparql_endpoint(url = "https://example.org", timeout = NA_real_), regexp = "positive")
  expect_error(sparql_endpoint(url = "https://example.org", timeout = c(1, 2)), regexp = "positive")
})

test_that("generic_endpoint inherits from sparql_endpoint", {
  ep <- generic_endpoint(url = "https://example.org/sparql")

  expect_true(S7::S7_inherits(ep, generic_endpoint))
  expect_true(S7::S7_inherits(ep, sparql_endpoint))
})
