test_that("pallas_graph_cache_dir returns an existing directory", {
  dir <- pallas_graph_cache_dir()

  expect_type(dir, "character")
  expect_true(dir.exists(dir))
})
