% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pathlib.R
\name{Path_rmdir}
\alias{Path_rmdir}
\title{Remove directories at the specified paths.}
\usage{
Path_rmdir(paths)
}
\arguments{
\item{paths}{A character vector of directory paths to remove.}
}
\description{
This function removes directories at the specified paths. It first checks if
the directories exist and are empty. Similar to Python's
\code{pathlib.Path.rmdir()} method.
}
\examples{
\dontrun{
Path_rmdir("empty_directory")
}
}
