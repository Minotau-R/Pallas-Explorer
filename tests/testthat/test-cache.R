test_that("pallas_graph_cache_dir returns an existing directory", {
  dir <- pallas_graph_cache_dir("PallasTestDefault")
  withr::defer(unlink(dir, recursive = TRUE))

  expect_type(dir, "character")
  expect_true(dir.exists(dir))
})

test_that("pallas_graph_cache_dir respects a custom dir_name", {
  dir <- pallas_graph_cache_dir("PallasTestCustom")
  withr::defer(unlink(dir, recursive = TRUE))

  expect_true(grepl("PallasTestCustom", dir, fixed = TRUE))
})

test_that("pallas_graph_cache_dir is idempotent", {
  dir1 <- pallas_graph_cache_dir("PallasTestIdempotent")
  withr::defer(unlink(dir1, recursive = TRUE))

  dir2 <- pallas_graph_cache_dir("PallasTestIdempotent")

  expect_equal(dir1, dir2)
  expect_true(dir.exists(dir2))
})
