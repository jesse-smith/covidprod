#' Get Paths to Last Downloaded Data for a REDcap Project
#'
#' `path_nit()` and `path_nca()` return file paths to the latest downloaded
#' dataset from the respective project. To download a dataset, use
#' \code{\link[covidprod:download_nit]{download_nit()}} or
#' \code{\link[covidprod:download_nca]{download_nca()}}.
#'
#' @inherit path_by_date params return
#'
#' @name paths
#'
#' @aliases path_nit path_nca
NULL

#' @rdname paths
#'
#' @export
path_nit <- function(date = NULL, force_latest = TRUE) {
  path_by_date(
    dir ="V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Data for R/nit/",
    date_format = "%Y-%m-%d",
    date_regex = "[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}",
    date = date,
    file_regex = ".*/nit_data_{date}[.]csv$",
    type = "file",
    force_latest = force_latest
  )
}

#' @rdname paths
#'
#' @export
path_nca <- function(date = NULL, force_latest = TRUE) {
  path_by_date(
    dir ="V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Data for R/nca/",
    date_format = "%Y-%m-%d",
    date_regex = "[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}",
    date = date,
    file_regex = ".*/nca_data_{date}[.]csv$",
    type = "file",
    force_latest = force_latest
  )
}

#' Return the Path to a NBS Investigations File
#'
#' `path_*` functions return paths to one or more files of interest
#' containing the supplied date in their names and ending with the supplied
#' file extension. These are convenience wrappers around `path_by_date()`, which
#' returns path that contain a specified date given additional parameters.
#'
#' \itemize{
#'   \item{`path_nit()` returns path(s) to NIT data}
#'   \item{`path_nca()` returns path(s) to NCA data}
#' }
#'
#' @param date A `Date` or string in the format "YYYY-MM-DD"
#'
#' @param dir The directory holding the files of interest
#'
#' @param date_format The format of the date in the file name; see
#'   \code{\link[base:format]{format()}} for more information
#'
#' @param date_regex A regular expression matching the date in the file name.
#'   This will hopefully be deprecated in the future, but is currently needed
#'   for extracting dates from the file path strings.
#'
#' @param file_regex A regular expression matching the file names of interest.
#'   The location of the date in the file name is specified using `"{date}"`, as
#'   in the glue package.
#'
#' @param type The file type(s) to return, one or more of `"any"`, `"file"`,
#' `"directory"`, `"symlink"`, `"FIFO"`, `"socket"`, `"character_device"` or
#' `"block_device"`.
#'
#' @param force_latest If multiple files with the given `date` are found,
#'   should the function return only the file with the latest creation date
#'   (`TRUE`, the default), or should it return all file paths (`FALSE`)?
#'
#' @return An `fs_path` character vector
path_by_date <- function(
  dir,
  date_format,
  date_regex,
  date = NULL,
  file_regex = ".*{date}.*",
  type = "any",
  force_latest = TRUE
) {

  # Create/clean `dir` path
  dir <- path_create(dir)

  # Format date - works if `NULL` as well by matching everything (".*")
  date_formatted <- date %>%
    lubridate::as_date() %>%
    format(format = date_format)

  if (rlang::is_empty(date_formatted)) {
    date_formatted <- ".*"
  }

  # Create regex pattern - get rid of duplicate wildcards
  file_regex <- file_regex %>%
    stringr::str_replace(
      pattern = "[{]date[}]",
      replacement = date_formatted
    ) %>%
    stringr::str_replace_all(
      pattern = "([.][*]){2,}",
      replacement = ".*"
    )

  # Get list of files matching date, or all files if `date = NULL`
  files <- dir %>%
    fs::dir_ls(type = type, regexp = file_regex) %>%
    vctrs::vec_sort()

  # If `date = NULL`, return the file with the latest date in the file name
  # Otherwise just sort `files` and return
  if (date_formatted == ".*") {
    files <- files %>%
      tibble::as_tibble_col("file") %>%
      dplyr::mutate(
        date = .data[["file"]] %>%
          fs::path_file() %>%
          stringr::str_extract(pattern = date_regex) %>%
          lubridate::fast_strptime(format = date_format, lt = FALSE) %>%
          lubridate::as_date()
      ) %>%
      dplyr::filter(.data[["date"]] == max(.data[["date"]], na.rm = TRUE)) %>%
      dplyr::pull(.data[["file"]])
  }

  if (vctrs::vec_size(files) > 1L && force_latest) {
    files <- files %>%
      fs::file_info() %>%
      dplyr::select("path", "birth_time") %>%
      dplyr::filter(
        .data[["birth_time"]] == max(.data[["birth_time"]], na.rm = TRUE)
      ) %>%
      dplyr::pull(.data[["path"]])
  }

  files
}
