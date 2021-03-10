#' Efficently Read Delimited Files
#'
#' `read_file_delim()` reads delimited files using
#' \code{\link[vroom:vroom]{vroom()}}. This allows the use of ALTREP columns,
#' which don't load data into memory until they are needed.
#'
#' By default, `read_file_delim()` does not attempt to guess column types and
#' reads all columns as character. This can be changed by setting
#' `col_types = vroom::cols(.default = vroom::col_guess())`. If columns are
#' guessed, the default is to use all rows; this can be changed by setting
#' `guess_max` to a different value.
#'
#' This saves a significant amount of time and space when loading data with many
#' rarely used columns.
#'
#' @inheritParams vroom::vroom
#'
#' @param ... Additional arguments to pass to \code{\link[vroom:vroom]{vroom()}}
#'
#' @return A `tibble` if reading one file; a list of `tibble`s if reading
#'   multiple
read_file_delim <- function(
  file,
  col_select = vroom::everything(),
  col_types = vroom::cols(.default = vroom::col_character()),
  na = c("", ".", "NA", "na", "Na", "N/A", "n/a", "N/a",
         "NULL", "null", "Null"),
  guess_max = .Machine$integer.max %/% 100L,
  delim = NULL,
  ...
) {
  vroom::vroom(
    file = path_create(file),
    delim = delim,
    col_types = col_types,
    col_select = col_select,
    na = na,
    guess_max = guess_max,
    ...
  )
}
