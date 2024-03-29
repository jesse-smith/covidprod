% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-date_tbl.R
\name{date_to_dt_dttm}
\alias{date_to_dt_dttm}
\title{Coerce Date Argument to \code{Date} or datetime}
\usage{
date_to_dt_dttm(date)
}
\arguments{
\item{date}{An object to be coerced to \code{Date} or datetime}
}
\value{
A \code{Date} or datetime object, depending on the input
}
\description{
\code{date_to_dt_dttm()} will coerce the input to a scalar \code{Date} or datetime
object if possible; if not, it will throw an error.
}
\details{
\code{date_to_dttm()} returns a \code{Date} when:
\itemize{
\item{a \code{string} coercable to \code{Date} does not contain time information}
\item{a \code{numeric} coerced to \code{Date} is closer to the current datetime than
the same \code{numeric} coerced to datetime}
}

\code{date_to_dt_dttm()} returns a datetime when:
\itemize{
\item{a \code{string} coercable to datetime contains time information}
\item{a \code{numeric} coerced to datetime is closer to the current datetime
than the same \code{numeric} coerced to \code{Date}}
}
}
\keyword{internal}
