#' Efficiently Write Delimited Files
#'
#' `write_file_delim()` writes delimited files using
#' \code{\link[vroom:vroom_write]{vroom_write()}}.
#'
#' By default, files are comma-delimited with missing values represented as
#' blank strings (`""`).
#'
#' When `clean = TRUE`, all atomic vectors are converted
#' to `character`, transliterated to ASCII, and
#' "\code{\link[stringr:str_squish]{squished}}". This is recommended unless
#' preserving whitespace is important, as it prevents ambiguity when reading
#' line breaks later.
#'
#' @param x Data frame or data frame extension to write to disk
#'
#' @param path Path or connection to write to
#'
#' @param delim Delimiter used to separate values. Defaults to `","` to write
#'   comma-separated value (CSV) files.
#'
#' @param na String used for missing values. Defaults to blank (`""`).
#'
#' @param clean Should atomic columns be cleaned before writing? This prevents
#'   ambiguity when reading in later.
#'
#' @param force Should any existing files be overwritten?
#'
#' @param ... Additional arguments to pass to
#'   \code{\link[vroom:vroom_write]{vroom_write()}}
#'
#' @return The input data
write_file_delim <- function(
  x,
  path,
  delim = ",",
  na = "",
  clean = TRUE,
  force = FALSE,
  ...
) {

  path <- path_create(path)

  if (!force && fs::file_exists(path)) {
    rlang::abort(
      "A file already exists at this location; to overwrite, set `force = TRUE`"
    )
  }

  if (clean) {
    x_write <- dplyr::mutate(
      x,
      dplyr::across(
        where(rlang::is_atomic),
        ~ .x %>%
          as.character() %>%
          chr_to_ascii() %>%
          stringr::str_squish()
      )
    )
  } else {
    x_write <- x
  }
  remove(x)



  vroom::vroom_write(
    x_write,
    path = path,
    delim = delim,
    na = na,
    ...
  )
}
