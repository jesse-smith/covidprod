coviData::log_start("productivity_report.log")
on.exit(coviData::log_end())

covidprod::download_nca(force = TRUE)
covidprod::download_nit(force = TRUE)


summary <- covidprod::summarize_prod()

asg_summary <- dplyr::select(summary, -dplyr::ends_with("_i"))

int_summary <- dplyr::select(summary, "date",  dplyr::ends_with("_i"))

wb <- openxlsx::createWorkbook()

wb$addWorksheet("Assignment Date")
wb$addWorksheet("Interview Date")

openxlsx::writeData(wb, sheet = "Assignment Date", asg_summary)
openxlsx::writeData(wb, sheet = "Interview Date", int_summary)

openxlsx::saveWorkbook(
  wb,
  file = fs::path(
    "V:/Productivity/Daily Report for Administration/test/",
    "productivity_report2.xlsx"
  ),
  overwrite = TRUE
)
