% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_file_delim.R
\name{write_file_delim}
\alias{write_file_delim}
\title{Efficiently Write Delimited Files}
\usage{
write_file_delim(
  x,
  path,
  delim = ",",
  na = "",
  clean = TRUE,
  force = FALSE,
  ...
)
}
\arguments{
\item{x}{Data frame or data frame extension to write to disk}

\item{path}{Path or connection to write to}

\item{delim}{Delimiter used to separate values. Defaults to \code{","} to write
comma-separated value (CSV) files.}

\item{na}{String used for missing values. Defaults to blank (\code{""}).}

\item{clean}{Should atomic columns be cleaned before writing? This prevents
ambiguity when reading in later.}

\item{force}{Should any existing files be overwritten?}

\item{...}{Additional arguments to pass to
\code{\link[vroom:vroom_write]{vroom_write()}}}
}
\value{
The input data
}
\description{
\code{write_file_delim()} writes delimited files using
\code{\link[vroom:vroom_write]{vroom_write()}}.
}
\details{
By default, files are comma-delimited with missing values represented as
blank strings (\code{""}).

When \code{clean = TRUE}, all atomic vectors are converted
to \code{character}, transliterated to ASCII, and
"\code{\link[stringr:str_squish]{squished}}". This is recommended unless
preserving whitespace is important, as it prevents ambiguity when reading
line breaks later.
}
