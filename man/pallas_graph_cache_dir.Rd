\name{pallas_graph_cache_dir}
\alias{pallas_graph_cache_dir}
\title{Get Package Cache Directory}
\usage{
pallas_graph_cache_dir(dir_name = "PallasGraph")
}
\arguments{
\item{dir_name}{A character scalar used as the application name for the cache directory.}
}
\value{A character scalar path to the cache directory. The directory is created if it does not exist.}
\description{Returns the user cache directory used by the package and ensures it exists.}
