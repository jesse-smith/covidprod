library(dplyr)

# dplyr::filter() - for filtering datasets

# NROW() - count rows

# as.numeric() - convert to numeric

# lubridate package - handling dates
#   - REDcap always outputs in year-month-day (YYYY-MM-DD)
#     - lubridate::ymd()
#   - Will sometimes add time
#     - lubridate::parse_date_time(orders = "ymdHMS")
