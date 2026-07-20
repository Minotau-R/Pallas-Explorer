#' Send 'SPARQL' query against an endpoint through HTTP request.
#'
#' @param url Character scalar. The SPARQL endpoint URL.
#' @param query Character scalar. A SPARQL query.
#' @param timeout Numeric scalar. Timeout in seconds.
#'
#' @return The parsed response in Json Format.
#' @export
sparql_query <- function(url, query, timeout = 60) {
  req <- httr2::request(url) |>
    httr2::req_url_query(query = query) |>
    httr2::req_timeout(timeout) |>
    httr2::req_headers(Accept = "application/sparql-results+json")

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}
