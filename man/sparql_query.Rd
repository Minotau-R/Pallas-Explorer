\name{sparql_query}
\alias{sparql_query}
\title{Send a SPARQL Query}
\usage{
sparql_query(url, query, timeout = 60)
}
\arguments{
\item{url}{Character scalar. The SPARQL endpoint URL.}
\item{query}{Character scalar. A SPARQL query.}
\item{timeout}{Numeric scalar. Timeout in seconds.}
}
\value{A data frame containing the parsed SPARQL response.}
\description{Send a SPARQL query against an endpoint through an HTTP request.}
