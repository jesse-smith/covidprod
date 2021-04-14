summarize_nca <- function(data = load_nca()) {

  # Create minimum date for filtering
  min_dt <- lubridate::as_date("2021-02-02")

  # Transform data
  data_t <- data %>%
    dplyr::mutate(
      answered_survey = .data[["resultofinter_5"]] == "1",
      called = !is.na(.data[["answer_4"]])
    ) %>%
    dplyr::filter(!.data[["answered_survey"]] | .data[["called"]]) %>%
    dplyr::transmute(
      # Assignment date
      assign_dt = .data[["assign_date"]] %>%
        lubridate::parse_date_time(orders = c("ymdHM", "ymdT")) %>%
        lubridate::as_date(),
      # Interview date
      interview_dt_1 = .data[["answer_4"]] %>%
        lubridate::parse_date_time(orders = c("ymdHM", "ymdT")) %>%
        lubridate::as_date(),
      interview_dt_2 = .data[["answer_9"]] %>%
        lubridate::parse_date_time(orders = c("ymdHM", "ymdT")) %>%
        lubridate::as_date(),
      interview_dt = dplyr::case_when(
        .data[["answer_3"]] == "Yes" ~ .data[["interview_dt_1"]]
      ),
      # Interviewed within 24 hours of assignment
      interviewed_in_24 = magrittr::is_weakly_less_than(
        .data[["interview_dttm"]],
        .data[["assign_dttm"]] + lubridate::hours(24L)
      ) & .data[["interview_success"]],
      # Interviewed within 48 hours of assignment
      interviewed_in_48 = magrittr::is_weakly_less_than(
        .data[["interview_dttm"]],
        .data[["assign_dttm"]] + lubridate::hours(48L)
      ) & .data[["interview_success"]],
      # Successful interview
      interview_success = dplyr::case_when(
        .data[["answer_3"]] == "Yes" ~ TRUE,
        .data[["answer_7"]] == "Yes" ~ TRUE,
        .data[["answer_3"]] == "No" ~ FALSE,
        .data[["answer_7"]] == "No" ~ FALSE,
        TRUE ~ NA
      )
    ) %>%
    # Arrange by date - has to happen during summary anyway
    dplyr::arrange(.data[["assign_dttm"]], .data[["interview_dttm"]])

  # Compute assignment date numbers
  asg_summary <- data_t %>%
    dplyr::group_by(.data[["assign_dt"]]) %>%
    dplyr::summarize(
      n_assigned = dplyr::n()
    )

  int_summary <- data_t %>%
    dplyr::group_by(.data)
}
