test_that("make_display_list maps local names to URIs", {
  result <- make_display_list(c(
    "http://ex.org/Book",
    "http://ex.org/Author"
  ))

  expect_equal(result$Author, "http://ex.org/Author")
  expect_equal(result$Book, "http://ex.org/Book")
})

test_that("make_display_list disambiguates colliding local names with the namespace", {
  result <- make_display_list(c(
    "http://xmlns.com/foaf/0.1/Person",
    "http://schema.org/Person"
  ))

  expect_length(result, 2)
  expect_equal(length(unique(names(result))), 2)
  expect_true(all(grepl("Person", names(result))))
})

test_that("make_display_list drops NA and empty values", {
  result <- make_display_list(c("http://ex.org/Book", NA, ""))

  expect_length(result, 1)
})

test_that("make_display_list returns an empty list for no input", {
  expect_equal(make_display_list(character()), list())
})

test_that("build_ont builds both classes and properties lists", {
  classes <- data.frame(class = "http://ex.org/Book", stringsAsFactors = FALSE)
  properties <- data.frame(property = "http://ex.org/title", stringsAsFactors = FALSE)

  ont <- build_ont(classes, properties)

  expect_equal(ont$classes$Book, "http://ex.org/Book")
  expect_equal(ont$properties$title, "http://ex.org/title")
})

test_that("build_ont handles NULL inputs", {
  ont <- build_ont(NULL, NULL)

  expect_equal(ont$classes, list())
  expect_equal(ont$properties, list())
})

test_that(".make_display_list_impl prefers a supplied label over the local name", {
  result <- .make_display_list_impl(
    c("http://ex.org/Book", "http://ex.org/Author"),
    c("A Book", NA_character_)
  )

  expect_equal(result[["A Book"]], "http://ex.org/Book")
  expect_equal(result$Author, "http://ex.org/Author")
})

test_that(".make_display_list_impl still disambiguates when labels collide", {
  result <- .make_display_list_impl(
    c("http://xmlns.com/foaf/0.1/Person", "http://schema.org/Person"),
    c("Person", "Person")
  )

  expect_length(result, 2)
  expect_equal(length(unique(names(result))), 2)
})

test_that(".make_display_list_impl de-duplicates repeated ids, keeping the first label", {
  result <- .make_display_list_impl(
    c("http://ex.org/Book", "http://ex.org/Book"),
    c("First", "Second")
  )

  expect_length(result, 1)
  expect_equal(result[["First"]], "http://ex.org/Book")
})
