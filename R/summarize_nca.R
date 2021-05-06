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
      # Create `reached`
      surveyed = dplyr::na_if(.data[["resultofinter_5"]] == "1", FALSE),
      interview1 = dplyr::if_else(.data[["answer_3"]] == "Yes", TRUE, NA),
      interview2 = (.data[["answer_7"]] == "Yes") %in% TRUE,
      reached = dplyr::coalesce(
        .data[["surveyed"]],
        .data[["interview1"]],
        .data[["interview2"]]
      ),
      # Create `interviewed`
      interviewed = dplyr::coalesce(
        .data[["interview1"]],
        .data[["interview2"]]
      ),
      # Create `attempt_dt`
      survey_dttm = lubridate::parse_date_time(
        .data[["answer_6"]],
        orders = c("ymdHM", "ymdT", "ymd")
      ),
      attempt1_dttm = lubridate::parse_date_time(
        .data[["answer_4"]],
        orders = c("ymdHM", "ymdT", "ymd")
      ),
      attempt2_dttm = lubridate::parse_date_time(
        .data[["answer_9"]],
        orders = c("ymdHM", "ymdT", "ymd")
      ),
      attempt_dttm = dplyr::coalesce(
        .data[["survey_dttm"]],
        .data[["attempt1_dttm"]],
        .data[["attempt2_dttm"]]
      ),
      attempt_dt = lubridate::as_date(.data[["attempt_dttm"]]),
      # Create `reach_dt`
      reach_dttm = dplyr::case_when(
        .data[["surveyed"]] ~ .data[["survey_dttm"]],
        .data[["interview1"]] ~ .data[["attempt1_dttm"]],
        .data[["interview2"]] ~ .data[["attempt2_dttm"]],
        TRUE ~ lubridate::NA_POSIXct_
      ),
      reach_dt = lubridate::as_date(.data[["reach_dttm"]]),
      # Create `complete`
      complete = .data[["reached"]] | !is.na(.data[["attempt_dt"]]),
      # Create `refused`
      refused = .data[["complete"]] & .data[["resultofinter"]] == "1",
      # Create `unable`
      unable = .data[["complete"]] & .data[["resultofinter_3"]] == "1",
      # Create `assign_dt`
      assign_dttm = lubridate::parse_date_time(
        .data[["assign_date"]],
        orders = c("ymdHM", "ymdT", "ymd")
      ),
      assign_dt = lubridate::as_date(.data[["assign_dttm"]]),
      # Create `hours_to_attempt`
      hours_to_attempt = difftime(
        .data[["attempt_dttm"]],
        .data[["assign_dttm"]],
        units = "hours"
      ),
      # Create `hours_to_reach`
      hours_to_reach = difftime(
        .data[["reach_dttm"]],
        .data[["assign_dttm"]],
        units = "hours"
      ),
      # Create interview time categories
      reached_in_24 = magrittr::and(
        .data[["reached"]],
        .data[["hours_to_reach"]] <= 24
      ),
      reached_in_48 = magrittr::and(
        .data[["reached"]],
        .data[["hours_to_reach"]] <= 48
      )
    ) %>%
    dplyr::select(
      -dplyr::ends_with("_dttm"),
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
      n_complete = sum(.data[["complete"]], na.rm = TRUE),
      n_incomplete = .data[["n_assigned"]] - .data[["n_complete"]],
      n_reached = sum(.data[["reached"]], na.rm = TRUE),
      n_interviewed = sum(.data[["interviewed"]], na.rm = TRUE),
      n_surveyed = sum(.data[["surveyed"]], na.rm = TRUE),
      n_unable = sum(.data[["unable"]], na.rm = TRUE),
      n_refused = sum(.data[["refused"]], na.rm = TRUE),
      n_reached_in_24 = sum(.data[["reached_in_24"]], na.rm = TRUE),
      n_reached_in_48 = sum(.data[["reached_in_48"]], na.rm = TRUE),
      # Percent of assigned
      pct_complete_a = .data[["n_complete"]] / .data[["n_assigned"]],
      pct_incomplete_a = 1 - .data[["pct_complete_a"]],
      pct_reached_a = .data[["n_reached"]] / .data[["n_assigned"]],
      pct_interviewed_a = .data[["n_interviewed"]] / .data[["n_assigned"]],
      pct_surveyed_a = .data[["n_surveyed"]] / .data[["n_assigned"]],
      pct_unable_a = .data[["n_unable"]] / .data[["n_assigned"]],
      pct_refused_a = .data[["n_refused"]] / .data[["n_assigned"]],
      pct_reached_in_24_a = magrittr::divide_by(
        .data[["n_reached_in_24"]],
        .data[["n_assigned"]]
      ),
      pct_reached_in_48_a = magrittr::divide_by(
        .data[["n_reached_in_48"]],
        .data[["n_assigned"]]
      ),
      # Percent of completed
      pct_reached_c = .data[["n_reached"]] / .data[["n_complete"]],
      pct_interviewed_c = .data[["n_interviewed"]] / .data[["n_complete"]],
      pct_surveyed_c = .data[["n_surveyed"]] / .data[["n_complete"]],
      pct_unable_c = .data[["n_unable"]] / .data[["n_complete"]],
      pct_refused_c = .data[["n_refused"]] / .data[["n_complete"]],
      pct_reached_in_24_c = magrittr::divide_by(
        .data[["n_reached_in_24"]],
        .data[["n_complete"]]
      ),
      pct_reached_in_48_c = magrittr::divide_by(
        .data[["n_reached_in_48"]],
        .data[["n_complete"]]
      ),
      # Median times
      median_hours_to_attempt = stats::median(
        .data[["hours_to_attempt"]],
        na.rm = TRUE
      ) %>%
        as.double() %>%
        round(1L),
      q90_hours_to_attempt = stats::quantile(
        .data[["hours_to_attempt"]],
        probs = 0.9,
        na.rm = TRUE,
        type = 8L
      ) %>%
        as.double() %>%
        round(1L),
      median_hours_to_reach = stats::median(
        .data[["hours_to_reach"]],
        na.rm = TRUE
      ) %>%
        as.double() %>%
        round(1L),
      q90_hours_to_reach = stats::quantile(
        .data[["hours_to_reach"]],
        probs = 0.9,
        na.rm = TRUE,
        type = 8L
      ) %>%
        as.double() %>%
        round(1L)
    ) %>%
    tidyr::complete(
      "assign_dt" = seq(min_dt, max_dt, by = 1L),
      fill = magrittr::set_names(
        as.list(rep(0, NROW(.) - 1L)),
        colnames(.)[2:NROW(.)]
      )
    )

  int_counts <- data_t %>%
    dplyr::filter(
      {{min_dt}} <= .data[["reach_dt"]],
      .data[["reach_dt"]] <= {{ max_dt }},
      .data[["interviewed"]]
    ) %>%
    dplyr::group_by(.data[["reach_dt"]]) %>%
    dplyr::summarize(
      n_interviewed_i = sum(.data[["interviewed"]], na.rm = TRUE),
      n_interviewed_in_24_i = sum(.data[["reached_in_24"]], na.rm = TRUE),
      n_interviewed_in_48_i = sum(.data[["reached_in_48"]], na.rm = TRUE),
      pct_interviewed_in_24_i = magrittr::divide_by(
        .data[["n_interviewed_in_24_i"]],
        .data[["n_interviewed_i"]]
      ),
      pct_interviewed_in_48_i = magrittr::divide_by(
        .data[["n_interviewed_in_48_i"]],
        .data[["n_interviewed_i"]]
      ),
      median_hours_to_interview_i = stats::median(
        .data[["hours_to_reach"]],
        na.rm = TRUE
      ) %>%
        as.double() %>%
        round(1L),
      q90_hours_to_interview_i = stats::quantile(
        .data[["hours_to_reach"]],
        probs = 0.9,
        na.rm = TRUE,
        type = 8L
      ) %>%
        as.double() %>%
        round(1L)
    ) %>%
    tidyr::complete(
      "reach_dt" = seq(min_dt, max_dt, by = 1L),
      fill = magrittr::set_names(
        as.list(rep(0, NROW(.) - 1L)),
        colnames(.)[2:NROW(.)]
      )
    )

  vctrs::vec_c(asg_counts[["assign_dt"]], int_counts[["reach_dt"]]) %>%
    vctrs::vec_unique() %>%
    vctrs::vec_sort() %>%
    tibble::as_tibble_col("date") %>%
    dplyr::left_join(asg_counts, by = c(date = "assign_dt")) %>%
    dplyr::left_join(int_counts, by = c(date = "reach_dt")) %>%
    tidyr::replace_na(
      c(list(lubridate::NA_Date_), as.list(rep(0, NROW(.) - 1L)))
    ) %>%
    as_date_tbl(date = if (is.null(data_dttm)) NA else data_dttm)
}
