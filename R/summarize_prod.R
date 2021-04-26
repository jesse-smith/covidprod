#' Summarize Productivity in NCA and NIT
#'
#' @param nca NCA data, as output by
#'   \code{\link[covidprod:load_nca]{load_nca()}}
#'
#' @param nit NIT data, as output by
#'   \code{\link[covidprod:load_nit]{load_nit()}}
#'
#' @return A `tibble` with one row per day since `"2020-02-02"`
#'
#' @export
summarize_prod <- function(nca = load_nca(), nit = load_nit()) {
  nca_summary <- summarize_nca(nca)
  nit_summary <- summarize_nit(nit)

  nca_dttm <- attr(nca_summary, "date", exact = TRUE)
  nit_dttm <- attr(nit_summary, "date", exact = TRUE)

  if (is.null(nca_dttm)) nca_dttm <- lubridate::NA_Date_
  if (is.null(nit_dttm)) nit_dttm <- lubridate::NA_Date_

  if (is.na(nca_dttm) && is.na(nit_dttm)) {
    data_dttm <- lubridate::NA_Date_
  } else if (is.na(nca_dttm)) {
    data_dttm <- nit_dttm
  } else if (is.na(nit_dttm)) {
    data_dttm <- nca_dttm
  } else {
    data_dttm <- pmin(
      lubridate::force_tz(nca_dttm, "UTC"),
      lubridate::force_tz(nit_dttm, "UTC")
    )
  }

  vctrs::vec_c(nca_summary[["date"]], nit_summary[["date"]]) %>%
    vctrs::vec_unique() %>%
    vctrs::vec_sort() %>%
    tibble::as_tibble_col("date") %>%
    dplyr::left_join(nca_summary, by = "date") %>%
    dplyr::left_join(nit_summary, by = "date") %>%
    tidyr::replace_na(
      c(list(lubridate::NA_Date_), as.list(rep(0, NROW(.) - 1L)))
    ) %>%
    as_date_tbl(date = data_dttm)
}
