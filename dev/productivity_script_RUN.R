coviData::log_start("productivity_report.log")
on.exit(coviData::log_end())

message("\n", Sys.time(), "\n")

coviData::ennotify_set_options(
  "Jesse.Smith@shelbycountytn.gov",
  "Allison.Plaxco@shelbycountytn.gov"
)
coviData::ennotify_context("Importing functions for productivity report")
import::from("magrittr", "%>%")
import::from("coviData", "ennotify_context")


ennotify_context("Creating functions for productivity report")
rename_cols <- function(data) {
  dplyr::rename_with(
    data,
    ~ .x %>%
      stringr::str_remove("^n_") %>%
      stringr::str_replace("^pct_", "% ") %>%
      stringr::str_remove("_[A-Za-z]$") %>%
      {dplyr::if_else(
        stringr::str_starts(., "q[0-9.]+_"),
        paste0(., " (", stringr::str_extract(., "(?<=q)[0-9.]+(?=_)"), "%)"),
        .
      )} %>%
      stringr::str_remove("^q[0-9.]+_") %>%
      janitor::make_clean_names(case = "title") %>%
      stringr::str_replace("\\s*Percent[_0-9]*$", "%)") %>%
      stringr::str_replace("\\s*(?=[0-9.]+%)", " (") %>%
      stringr::str_replace("^\\s*Percent", "%")
  )
}

ennotify_context("Downloading data for productivity report")
covidprod::download_nca(force = TRUE)
covidprod::download_nit(force = TRUE)


ennotify_context("Summarizing data for productivity report")
summary <- covidprod::summarize_prod()

ennotify_context("Splitting productivity data into sheets")
asg_summary <- summary %>%
  dplyr::select(-dplyr::ends_with(c("_i", "_a", "_c"))) %>%
  rename_cols()

asg_summary_a <- summary %>%
  dplyr::select("date", "n_assigned", dplyr::ends_with("_a")) %>%
  rename_cols()

asg_summary_c <- summary %>%
  dplyr::select("date", "n_complete", dplyr::ends_with("_c")) %>%
  rename_cols()

int_summary <- summary %>%
  dplyr::select("date", dplyr::ends_with("_i")) %>%
  rename_cols()

ennotify_context("Creating productivity workbook")
wb <- openxlsx::createWorkbook()

wb$addWorksheet("Assignment Date")
wb$addWorksheet("Assignment Date (% Assigned)")
wb$addWorksheet("Assignment Date (% Completed)")
wb$addWorksheet("Interview Date")

openxlsx::writeData(wb, sheet = "Assignment Date", asg_summary)
openxlsx::writeData(wb, sheet = "Assignment Date (% Assigned)", asg_summary_a)
openxlsx::writeData(wb, sheet = "Assignment Date (% Completed)", asg_summary_c)
openxlsx::writeData(wb, sheet = "Interview Date", int_summary)

openxlsx::addStyle(
  wb,
  sheet = "Assignment Date (% Assigned)",
  style = openxlsx::createStyle(numFmt = "0.0%"),
  rows  = 1:(100 + NROW(asg_summary_a)),
  cols  = 3:NCOL(asg_summary_a),
  gridExpand = TRUE,
  stack = TRUE
)

openxlsx::addStyle(
  wb,
  sheet = "Assignment Date (% Completed)",
  style = openxlsx::createStyle(numFmt = "0.0%"),
  rows  = 1:(100 + NROW(asg_summary_c)),
  cols  = 3:NCOL(asg_summary_c),
  gridExpand = TRUE,
  stack = TRUE
)

openxlsx::addStyle(
  wb,
  sheet = "Interview Date",
  style = openxlsx::createStyle(numFmt = "0.0%"),
  rows = 1:(100 + NROW(int_summary)),
  cols = stringr::str_which(colnames(int_summary), "^%"),
  gridExpand = TRUE,
  stack = TRUE
)

purrr::walk(
  names(wb),
  ~ openxlsx::setColWidths(wb, sheet = .x, cols = 2:100, widths = "auto")
)

ennotify_context("Saving productivity workbook")
openxlsx::saveWorkbook(
  wb,
  file = fs::path(
    "V:/Productivity/Daily Report for Administration/test/",
    "productivity_report2.xlsx"
  ),
  overwrite = TRUE
)
