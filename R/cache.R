#' @noRd
pallas_graph_cache_dir <- function(dir_name = "PallasGraph") {
  dir <- tools::R_user_dir(dir_name, which = "cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}
