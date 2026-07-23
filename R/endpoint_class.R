#' 'SPARQL' endpoint (base class)
#'
#' The base S7 class every endpoint backend inherits from. You will
#' not normally call `sparql_endpoint()` directly -- use a specific
#' constructor like `new_generic_endpoint()`.
#'
#' @param url Character. The SPARQL endpoint URL (the address a query
#'   is sent to).
#' @param name Character. A short label for the endpoint, used for
#'   caching and messages.
#' @param headers A named list of extra HTTP headers sent with every
#'   request (for example a custom `User-Agent`).
#' @param timeout Numeric. Request timeout in seconds.
#'
#' @export
sparql_endpoint <- S7::new_class(
  "sparql_endpoint",
  properties = list(
    url = S7::new_property(
      S7::class_character,
      validator = function(value) {
        if (length(value) != 1 || !nzchar(value)) "must be a single non-empty string"
      }
    ),
    name = S7::new_property(
      S7::class_character,
      default = quote(NA_character_),
      validator = function(value) {
        if (length(value) != 1) "must be a single string (or NA)"
      }
    ),
    headers = S7::new_property(
      S7::class_list,
      default = quote(list()),
      validator = function(value) {
        if (length(value) > 0 && (is.null(names(value)) || any(!nzchar(names(value))))) {
          "must be a fully named list"
        }
      }
    ),
    timeout = S7::new_property(
      S7::class_numeric,
      default = 60,
      validator = function(value) {
        if (length(value) != 1 || is.na(value) || value <= 0) "must be a single positive number"
      }
    )
  )
)

#' Generic 'SPARQL' 1.1 backend
#'
#' The fallback backend used for any endpoint that isn't registered
#' under a more specific class.
#'
#' @param url,name,headers,timeout See [sparql_endpoint()].
#' @export
generic_endpoint <- S7::new_class("generic_endpoint", parent = sparql_endpoint)

#' Discover the classes used by an endpoint
#' @param endpoint A `sparql_endpoint` object.
#' @param ... Passed on to methods (commonly `limit`).
#' @return A data frame with columns `class` and `n`.
#' @export
ep_classes <- S7::new_generic("ep_classes", "endpoint")

#' Discover the properties (predicates) used by an endpoint
#' @inheritParams ep_classes
#' @return A data frame with columns `property` and `n`.
#' @export
ep_properties <- S7::new_generic("ep_properties", "endpoint")

#' Discover which classes are linked by which properties
#' @inheritParams ep_classes
#' @return A data frame with columns `from_class`, `property`, `to_class`, `n`.
#' @export
ep_class_links <- S7::new_generic("ep_class_links", "endpoint")
