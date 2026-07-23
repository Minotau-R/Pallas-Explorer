test_that("sparql_endpoint validator rejects a bad url", {
  expect_error(sparql_endpoint(url = ""))
  expect_error(sparql_endpoint(url = c("a", "b")))
})
