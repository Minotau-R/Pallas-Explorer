test_that("sparql_json_to_df handles a normal SELECT result", {
  parsed <- list(
    head = list(vars = list("s", "p")),
    results = list(bindings = list(
      list(s = list(type = "uri", value = "http://ex.org/1"),
           p = list(type = "uri", value = "http://ex.org/hasName")),
      list(s = list(type = "uri", value = "http://ex.org/2"),
           p = list(type = "uri", value = "http://ex.org/hasAge"))
    ))
  )

  result <- .sparql_json_to_df(parsed)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("s", "p"))
  expect_equal(result$s, c("http://ex.org/1", "http://ex.org/2"))
})

test_that("sparql_json_to_df fills missing optional values with NA", {
  parsed <- list(
    head = list(vars = list("s", "label")),
    results = list(bindings = list(
      list(s = list(type = "uri", value = "http://ex.org/1"))
    ))
  )

  result <- .sparql_json_to_df(parsed)

  expect_true(is.na(result$label[1]))
})

test_that("sparql_json_to_df returns a correctly-shaped empty data frame", {
  parsed <- list(
    head = list(vars = list("s", "p")),
    results = list(bindings = list())
  )

  result <- .sparql_json_to_df(parsed)

  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("s", "p"))
})

test_that("sparql_json_to_df handles ASK queries", {
  parsed <- list(head = list(), boolean = TRUE)

  expect_equal(.sparql_json_to_df(parsed)$boolean, TRUE)
})
