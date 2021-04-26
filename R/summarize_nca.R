summarize_nca <- function(data = load_nca()) {

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

  # Transform data
  data_t <- data %>%
    dplyr::transmute(
      surveyed = dplyr::na_if(.data[["resultofinter_5"]] == "1", FALSE),
      attempted = .data[["resultofinter_4"]] == "0",
      refused = .data[["resultofinter"]] == "1",
      unable = .data[["resultofinter_3"]] == "1",
      interview1 = dplyr::if_else(.data[["answer_3"]] == "Yes", TRUE, NA),
      interview2 = (.data[["answer_7"]] == "Yes") %in% TRUE,
      survey_dttm = lubridate::parse_date_time(
        dplyr::if_else(
          .data[["surveyed"]],
          .data[["answer_6"]],
          NA_character_
        ),
        orders = c("ymdHM", "ymdT", "ymd")
      ),
      interview1_dttm = lubridate::parse_date_time(
        dplyr::if_else(
          .data[["interview1"]] %in% TRUE,
          .data[["answer_4"]],
          NA_character_
        ),
        orders = c("ymdHM", "ymdT", "ymd")
      ),
      interview2_dttm = lubridate::parse_date_time(
        dplyr::if_else(
          .data[["interview2"]],
          .data[["answer_9"]],
          NA_character_
        ),
        orders = c("ymdHM", "ymdT", "ymd")
      ),
      assign_dttm = lubridate::parse_date_time(
        .data[["assign_date"]],
        orders = c("ymdHM", "ymdT", "ymd")
      ),
      assign_dt = lubridate::as_date(.data[["assign_dttm"]]),
      interviewed = dplyr::coalesce(
        .data[["surveyed"]],
        .data[["interview1"]],
        .data[["interview2"]]
      ),
      interview_dttm = dplyr::coalesce(
        .data[["survey_dttm"]],
        .data[["interview1_dttm"]],
        .data[["interview2_dttm"]]
      ),
      interview_dt = lubridate::as_date(.data[["interview_dttm"]]),
      hours_to_interview = difftime(
        .data[["interview_dttm"]],
        .data[["assign_dttm"]],
        units = "hours"
      ),
      interviewed_in_24 = .data[["hours_to_interview"]] <= 24,
      interviewed_in_48 = .data[["hours_to_interview"]] <= 48,
      interviewed_btwn_24_48 = magrittr::and(
        !.data[["interviewed_in_24"]],
         .data[["interviewed_in_48"]]
      )
    ) %>%
    dplyr::select(
      -c("assign_dttm", "interview_dttm"),
      -dplyr::starts_with(c("interview1", "interview2"))
    )

  remove(data)

  asg_counts <- data_t %>%
    dplyr::filter(
      {{ min_dt }} <= .data[["assign_dt"]],
      .data[["assign_dt"]] <= {{ max_dt }},
      !is.na(.data[["assign_dt"]])
    ) %>%
    dplyr::group_by(.data[["assign_dt"]]) %>%
    dplyr::summarize(
      n_assigned = dplyr::n(),
      n_attempted = sum(.data[["attempted"]], na.rm = TRUE),
      n_refused = sum(.data[["refused"]], na.rm = TRUE),
      n_unable = sum(.data[["unable"]], na.rm = TRUE),
      n_interviewed = sum(.data[["interviewed"]], na.rm = TRUE),
      n_interviewed_in_48 = sum(.data[["interviewed_in_48"]], na.rm = TRUE),
      n_interviewed_in_24 = sum(.data[["interviewed_in_24"]], na.rm = TRUE),
      n_interviewed_btwn_24_48 = magrittr::subtract(
        .data[["n_interviewed_in_48"]],
        .data[["n_interviewed_in_24"]]
      ),
      pct_attempted = .data[["n_attempted"]] / .data[["n_assigned"]],
      pct_interviewed = .data[["n_interviewed"]] / .data[["n_assigned"]],
      pct_interviewed_in_24 = magrittr::divide_by(
        .data[["n_interviewed_in_24"]],
        .data[["n_assigned"]]
      )
    ) %>%
    tidyr::complete(
      "assign_dt" = seq(min_dt, max_dt, by = 1L),
      fill = as.list(rep(0, NROW(.) - 1L))
    )

  int_counts <- data_t %>%
    dplyr::filter(
      {{min_dt}} <= .data[["interview_dt"]],
      .data[["interview_dt"]] <= {{ max_dt }}
    ) %>%
    dplyr::group_by(interview_dt) %>%
    dplyr::summarize(
      n_interviewed_i = sum(.data[["interviewed"]], na.rm = TRUE),
      n_interviewed_in_48_i = sum(.data[["interviewed_in_48"]], na.rm = TRUE),
      n_interviewed_in_24_i = sum(.data[["interviewed_in_24"]], na.rm = TRUE),
      n_interviewed_btwn_24_48_i = magrittr::subtract(
        .data[["n_interviewed_in_48_i"]],
        .data[["n_interviewed_in_24_i"]]
      ),
      pct_interviewed_in_24_i = magrittr::divide_by(
        .data[["n_interviewed_in_24_i"]],
        .data[["n_interviewed_i"]]
      )
    ) %>%
    tidyr::complete(
      "interview_dt" = seq(min_dt, max_dt, by = 1L),
      fill = as.list(rep(0, NROW(.) - 1L))
    )

  vctrs::vec_c(asg_counts[["assign_dt"]], int_counts[["interview_dt"]]) %>%
    vctrs::vec_unique() %>%
    vctrs::vec_sort() %>%
    tibble::as_tibble_col("date") %>%
    dplyr::left_join(asg_counts, by = c(date = "assign_dt")) %>%
    dplyr::left_join(int_counts, by = c(date = "interview_dt")) %>%
    tidyr::replace_na(
      c(list(lubridate::NA_Date_), as.list(rep(0, NROW(.) - 1L)))
    ) %>%
    as_date_tbl(date = if (is.null(data_dttm)) NA else data_dttm)
}
