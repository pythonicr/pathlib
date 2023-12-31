% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pathlib.R
\name{Path_exists}
\alias{Path_exists}
\title{Check if the provided paths exist.}
\usage{
Path_exists(paths)
}
\arguments{
\item{paths}{A character vector of file paths.}
}
\value{
A logical vector indicating the existence of each path.
}
\description{
This function checks if the specified files or directories exist and returns
a logical vector indicating their existence, similar to Python's
\code{pathlib.Path.exists()} method.
}
\examples{
\dontrun{
Path_exists("file.txt")
}
}
