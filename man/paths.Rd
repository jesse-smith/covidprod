% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/path_by_date.R
\name{paths}
\alias{paths}
\alias{path_nit}
\alias{path_nca}
\title{Get Paths to Last Downloaded Data for a REDcap Project}
\usage{
path_nit(date = NULL, force_latest = TRUE)

path_nca(date = NULL, force_latest = TRUE)
}
\arguments{
\item{date}{A \code{Date} or string in the format "YYYY-MM-DD"}

\item{force_latest}{If multiple files with the given \code{date} are found,
should the function return only the file with the latest creation date
(\code{TRUE}, the default), or should it return all file paths (\code{FALSE})?}
}
\value{
An \code{fs_path} character vector
}
\description{
\code{path_nit()} and \code{path_nca()} return file paths to the latest downloaded
dataset from the respective project. To download a dataset, use
\code{\link[covidprod:download_nit]{download_nit()}} or
\code{\link[covidprod:download_nca]{download_nca()}}.
}
