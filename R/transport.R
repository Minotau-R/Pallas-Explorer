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
  status_code <- httr2::resp_status(resp)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop(
        sprintf("SPARQL request to %s failed.\nReason: %s", url, conditionMessage(e)),
        call. = FALSE
      )
    }
  )

  tryCatch(
    parsed_resp <- httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) {
      stop(
        "Failed to parse SPARQL response as JSON. The endpoint may have returned an unexpected format (e.g., HTML).",
        call. = FALSE
      )
    }
  )

  tryCatch(
    .sparql_json_to_df(parsed = parsed_resp),
     error = function(e) {
      stop(
        "Transformation into Data Frame failed on the response.",
        call. = FALSE
      )
    }
  )
}


#' @noRd
.sparql_json_to_df <- function(parsed) {
  if (!is.null(parsed$boolean)) {
    return(data.frame(boolean = parsed$boolean))
  }

  vars <- unlist(parsed$head$vars)
  bindings <- parsed$results$bindings

  if (length(bindings) == 0) {
    return(as.data.frame(
      stats::setNames(lapply(vars, function(x) character(0)), vars)
    ))
  }

  rows <- lapply(bindings, function(b) {
    vals <- lapply(vars, function(v) {
      cell <- b[[v]]
      if (is.null(cell)) NA_character_ else cell$value
    })
    names(vals) <- vars
    as.data.frame(vals, stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}
