#' `date_tbl`: A `tibble` with a `date`/`datetime` Attribute
#'
#' @description
#' `date_tbl()` is a subclass of `tibble` that stores a `date` or `datetime`
#' attribute as metadata. This is useful, for example, in tracking the report
#' date of a dataset.
#'
#' `as_date_tbl()` converts an object to a `date_tbl`
#'
#' `is_date_tbl()` tests whether an object is a `date_tbl`
#'
#' `new_date_tbl()` constructs a `date_tbl` from a `tibble`-like object and a
#'   date with minimal checking
#'
#' `validate_date_tbl()` validates the properties of a `date_tbl`
#'
#' @param x A `date_tbl`, `tibble`-like object, or an object to test for
#'   inheritance from `date_tbl`. In `as_date_tbl()`, this can be anything
#'   coercable to a `tibble`; `new_date_tbl()` does less work and is
#'   somewhat stricter about the input. `is_date_tbl()` and
#'   `validate_date_tbl()` will take any object, but will return `FALSE` or an
#'   error (respectively) if the object is not a properly created `date_tbl`.
#'
#' @param date A `Date` or datetime (`POSIXt`) object of length 1, or a string
#'   that can be coerced by \code{\link[lubridate:as_date]{as_date()}} or
#'   \code{\link[lubridate:as_datetime]{as_datetime()}}
#'
#' @inheritParams tibble::as_tibble
#'
#' @inheritParams tibble::is_tibble
#'
#' @inheritParams tibble::new_tibble
#'
#' @return A `date_tbl`
#'
#' @name class-date_tbl
#'
#' @aliases as_date_tbl is_date_tbl new_date_tbl validate_date_tbl
NULL

#' @rdname class-date_tbl
#'
#' @keywords internal
as_date_tbl <- function(
  x,
  date,
  .rows = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal"),
  rownames = pkgconfig::get_config("tibble::rownames", NULL)
) {
  x %>%
    tibble::as_tibble(
      .rows = .rows,
      .name_repair = .name_repair,
      rownames = rownames
    ) %>%
    new_date_tbl(date = date) %>%
    validate_date_tbl()
}

#' @rdname class-date_tbl
#'
#' @keywords internal
is_date_tbl <- function(x) {
  rlang::inherits_all(x, c("date_tbl", "tbl_df", "tbl", "data.frame"))
}

#' @rdname class-date_tbl
#'
#' @keywords internal
new_date_tbl <- function(x, date, nrow = vctrs::vec_size(x)) {

  date <- date_to_dt_dttm(date)

  tibble::new_tibble(x, date = date, nrow = nrow, class = "date_tbl")
}

#' @rdname class-date_tbl
#'
#' @keywords internal
validate_date_tbl <- function(x) {

  # Check class
  if (!is_date_tbl(x)) {
    rlang::abort("`x` must be of class `date_tbl`")
  }

  # Check date attribute
  validate_date_attr(x)

  date <- attr(x, "date", exact = TRUE)

  # Check date type
  validate_date_type(date)

  # Check date range
  validate_date_range(date)

  # Validate tibble
  tibble::validate_tibble(x)
}

#' Coerce Date Argument to `Date` or datetime
#'
#' `date_to_dt_dttm()` will coerce the input to a scalar `Date` or datetime
#' object if possible; if not, it will throw an error.
#'
#' `date_to_dttm()` returns a `Date` when:
#' \itemize{
#'   \item{a `string` coercable to `Date` does not contain time information}
#'   \item{a `numeric` coerced to `Date` is closer to the current datetime than
#'   the same `numeric` coerced to datetime}
#' }
#'
#' `date_to_dt_dttm()` returns a datetime when:
#' \itemize{
#'   \item{a `string` coercable to datetime contains time information}
#'   \item{a `numeric` coerced to datetime is closer to the current datetime
#'   than the same `numeric` coerced to `Date`}
#' }
#'
#' @param date An object to be coerced to `Date` or datetime
#'
#' @return A `Date` or datetime object, depending on the input
#'
#' @keywords internal
date_to_dt_dttm <- function(date) {

  if (is.na(date) && lubridate::is.POSIXt(date)) {
    return(lubridate::NA_POSIXct_)
  } else if (is.na(date)) {
    return(lubridate::NA_Date_)
  }

  date_is_dt_dttm <- lubridate::is.Date(date) || lubridate::is.POSIXt(date)
  if (date_is_dt_dttm) return(date)

  validate_date_type(date)

  date_is_numeric <- magrittr::or(
    rlang::is_scalar_integer(date),
    rlang::is_scalar_double(date)
  )
  date_is_string <- rlang::is_scalar_character(date)

  if (date_is_numeric) {
    type <- num_dt_dttm(date)
  } else {
    type <- str_dt_dttm(date)
  }

  if (is.na(type)) {
    rlang::abort("`date` is not coercable to `Date` or datetime")
  } else if (type == "date") {
    lubridate::as_date(date)
  } else if (type == "datetime") {
    lubridate::as_datetime(date)
  } else {
    rlang::abort("`date` is not coercable to `Date` or datetime")
  }
}

#' Validate than an Object has a `"date"` Attribute
#'
#' @param x An object to check
#'
#' @return The input object if valid; otherwise, throws an error
#'
#' @keywords internal
validate_date_attr <- function(x) {
  date <- attr(x, "date", exact = TRUE)
  has_date <- !is.null(date)
  if (!has_date) {
    rlang::abort("`x` must have a `date` attribute")
  }

  x
}

#' Validate That a Potential `date` is a Valid Type
#'
#' `validate_date_type()` checks that a potential `date` is either a `Date`,
#' datetime, string, or numeric, and that it is a scalar.
#'
#' @param date Potential `date` to check
#'
#' @return The input if valid; otherwise, throws an error
#'
#' @keywords internal
validate_date_type <- function(date) {
  date_is_na <- is.na(date)
  date_is_dt_dttm <- lubridate::is.Date(date) || lubridate::is.POSIXt(date)
  date_is_chr <- rlang::is_character(date)
  date_is_num <- rlang::is_double(date) || rlang::is_integer(date)
  date_is_scalar <- rlang::is_scalar_vector(date)

  date_is_valid <- rlang::is_true(
    (date_is_na || date_is_dt_dttm || date_is_chr || date_is_num) &&
      date_is_scalar
  )

  if (!date_is_valid) {
    rlang::abort(
      "`date` must be a scalar `Date` or datetime, or coercable to one"
    )
  }

  date
}

#' Validate That a `date` falls in Valid Range
#'
#' `validate_date_range()` validates that a date falls between `"2020-01-01"`
#' and the current date (inclusive).
#'
#' @param date A `Date` or datetime
#'
#' @return The input if valid; otherwise, throws an error
#'
#' @keywords internal
validate_date_range <- function(date) {

  date_range_is_valid <- all(
    lubridate::as_date("2020-01-01") <= date,
    date <= lubridate::now()
  ) || is.na(date)

  if (!date_range_is_valid) {
    rlang::abort(
      paste0(
        "`date` must be between 2020-01-01 and today (",
        lubridate::today(),
        ")"
      )
    )
  }

  date
}


#' Check That a String can be parsed as a datetime
#'
#' @param date A string to check for date information
#'
#' @return A scalar `logical`
#'
#' @keywords internal
str_is_dttm <- function(date) {
  is.null(rlang::catch_cnd(lubridate::parse_date_time(date, orders = "ymdT")))
}

#' Check that a String Can Be Parsed as a Date (and not a datetime)
#'
#' @param date A string to check for date information
#'
#' @return A scalar `logical`
#'
#' @keywords internal
str_is_dt <- function(date) {
  magrittr::and(
    is.null(rlang::catch_cnd(lubridate::parse_date_time(date, orders = "ymd"))),
    !str_is_dttm(date)
  )
}

#' Get Best Parsing Type for a Date String
#'
#' `str_dt_dttm()` returns `"date"` if the string can be parsed as a `Date` and
#' contains no time information. It returns `"datetime"` if the string can be
#' parsed as a datetime and contains time information. Otherwise, it returns
#' `NA_character_`.
#'
#' @param date A potential date encoded as a string
#'
#' @return `"date"`, `"datetime"`, or `NA_character_`
#'
#' @keywords internal
str_dt_dttm <- function(date) {

  if (is.na(date)) return(NA_character_)

  if (!rlang::is_scalar_character(date)) rlang::abort("`date` must be a string")

  if (str_is_dttm(date)) {
    "datetime"
  } else if (str_is_dt(date)) {
    "date"
  } else {
    NA_character_
  }
}

#' Get Best Type for a Date Numeric
#'
#' `num_dt_dttm()` returns the type that results in a datetime closer to the
#' current datetime. It returns `NA_character_` if the input is a missing value.
#'
#' @param date A date in numeric format
#'
#' @return `"date"`, `"datetime"`, or `NA_character_`
num_dt_dttm <- function(date) {
  is_scalar_numeric <- magrittr::or(
    rlang::is_scalar_double(date),
    rlang::is_scalar_integer(date)
  )

  if (is.na(date)) return(NA_character_)

  if (!is_scalar_numeric) rlang::abort("`date` must be a scalar numeric")

  dt <- lubridate::as_date(date)
  dttm <- lubridate::as_datetime(date)

  now <- lubridate::now(tz = "UTC")

  dt_dist <- lubridate::as_datetime(dt) %>%
    difftime(now, tz = "UTC", units = "sec") %>%
    as.integer() %>%
    abs()

  dttm_dist <- dttm %>%
    difftime(now, tz = "UTC", units = "sec") %>%
    as.integer() %>%
    abs()

  if (dt_dist <= dttm_dist) {
    "date"
  } else if (dt_dist > dttm_dist) {
    "datetime"
  } else {
    NA_character_
  }
}
