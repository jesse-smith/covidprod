summarize_nit <- function(data = load_nit()) {

  # Create minimum date for filtering
  min_dt <- lubridate::as_date("2021-02-02")

  # Create maximum date for filtering
  data_dttm <- attr(data, "date", exact = TRUE)
  if (lubridate::is.POSIXt(data_dttm)) {
    time <- hms::trunc_hms(hms::as_hms(data_dttm), secs = 1L)
    five_pm <- hms::parse_hm("17:00")
    max_dt <- if (time >= five_pm) lubridate::today() else lubridate::today()-1L
  } else {
    max_dt <- lubridate::today() - 1L
  }

  data %>%
    # Create needed variables
    dplyr::transmute(
      interview_dt = .data[["date"]] %>%
        lubridate::parse_date_time(orders = c("ymdHM", "ymdT", "ymd")) %>%
        lubridate::as_date(),
      n_contacts = as.integer(.data[["numb_contacts_16"]]),

    ) %>%
    # Filter to valid dates
    dplyr::filter(
      {{ min_dt }} <= .data[["interview_dt"]],
      .data[["interview_dt"]] <= {{ max_dt }}
    ) %>%
    # Group calculations by date
    dplyr::group_by(.data[["interview_dt"]]) %>%
    # Summary statistics
    dplyr::summarize(
      n_contacts_i = sum(.data[["n_contacts"]], na.rm = TRUE),

    ) %>%
    # Rename date variable to match `summarize_nca()`
    dplyr::rename(date = "interview_dt") %>%
    # Fill in implicitly missing dates
    tidyr::complete(
      "date" = seq(min_dt, max_dt, by = 1L),
      fill = as.list(rep(0, NROW(.) - 1L))
    ) %>%
    # Coerce to date_tbl
    as_date_tbl(date = if (is.null(data_dttm)) NA else data_dttm)
}
