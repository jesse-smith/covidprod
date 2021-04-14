coviData::log_start("productivity_report.log")
on.exit(coviData::log_end())

library(tidyverse)
library(covidprod)

# these download all of the project data

nca_date <- path_nca() %>%
  fs::path_file() %>%
  fs::path_ext_remove() %>%
  stringr::str_extract("[0-9]{1,4}.?[0-9]{1,2}.?[0-9]{1,4}") %>%
  lubridate::as_date()

nit_date <- path_nit() %>%
  fs::path_file() %>%
  fs::path_ext_remove() %>%
  stringr::str_extract("[0-9]{1,4}.?[0-9]{1,2}.?[0-9]{1,4}") %>%
  lubridate::as_date()

if (nca_date != lubridate::today()) download_nca()
if (nit_date != lubridate::today()) download_nit()

# this loads the data
NCA <- load_nca()
NIT <- load_nit()

# this pulls the data based on the term "yesterday"
yesterday <- lubridate::today()-1

NCA$assign_date <- lubridate::parse_date_time(NCA$assign_date, orders= "ymdHM")

NCA$interviewtime <- lubridate::parse_date_time(NCA$answer_4, orders= c("ymdHM", "ymdHMS"))

date_asg_int <- subset (NCA, select= c(record_id,assign_date,answer_3,interviewtime))


date_asg_int$timetoint <- difftime(date_asg_int$interviewtime,
                                   date_asg_int$assign_date, units = c("hours"))

interview <- subset(date_asg_int, answer_3=="Yes")

# number assigned
date_asg_int$assign_dateonly <- lubridate::as_date(date_asg_int$assign_date)
assigned<-sum(date_asg_int$assign_dateonly==yesterday, na.rm = TRUE)


# number interviewed
date_asg_int$interviewtimeonly <- lubridate::as_date(date_asg_int$interviewtime)
interviewed <- sum(date_asg_int$answer_3== "Yes" &
                     date_asg_int$interviewtimeonly==yesterday, na.rm = TRUE)


interview$interviewtimeonly <- lubridate::as_date(interview$interviewtime)
int_yest <- subset(interview, interview$interviewtimeonly == yesterday)



# interviewed in 24 hours
interviewed24 <- sum(int_yest$timetoint <= 24, na.rm = TRUE)

# number interviewed between 24 and 48 hours
interviewed24_48 <- sum(int_yest$timetoint >= 24 & int_yest$timetoint <=48, na.rm = TRUE)

# number interviewed in 48 hours
interviewed48 <- sum(int_yest$timetoint<=48, na.rm = TRUE)

# percent interviewed in 24 hours
percent24 <- (interviewed24/interviewed)

NIT <- load_nit()

NIT$dateap <- lubridate::parse_date_time(NIT$date, orders= "ymdHM")


# number of contacts identified
contacts <- subset(NIT, select= c(dateap, numb_contacts_16))

contacts$dateaponly <- lubridate::as_date(contacts$dateap)
contacts_yest <- subset (contacts,contacts$dateaponly == yesterday)

contacts_yest$numb_contacts_16<- as.numeric(contacts_yest$numb_contacts_16)
contact_ident<- sum(contacts_yest$numb_contacts_16, na.rm = TRUE)



# number unable to reach
unable<- subset(NIT, select= c(resultofinter_3, dateap))

unable$dateaponly <- lubridate::as_date(unable$dateap)
unable_yest <- subset(unable, unable$dateaponly == yesterday)

unable_yest$resultofinter_3 <- as.numeric(unable_yest$resultofinter_3)
unable_ident<- sum(unable_yest$resultofinter_3, na.rm = TRUE)


# number refused to interview
refused<- subset(NIT, select= c(resultofinter, dateap))

refused$dateaponly <- lubridate::as_date(refused$dateap)
refused_yest <- subset(refused, refused$dateaponly == yesterday)


refused_yest$resultofinter <- as.numeric(refused_yest$resultofinter)
refused_ident <- sum (refused_yest$resultofinter, na.rm = TRUE)


# total cases closed
total_cs_clsd <- (interviewed+refused_ident+unable_ident)



# now, write the data into the productivity file
row_to_add <- tibble(
  Day = weekdays(Sys.Date() - 1L),
  Date = Sys.Date() - 1L,
  "Number of Investigations Assigned"= assigned,
  "Number of Cases Interviewed" = interviewed,
  "Number Interviewed within 24 hours of assignment"= interviewed24,
  "% Interviewed w/in 24 hrs of assigned"  = percent24,
  "Number Interviewed between 24 and 48 hours"  = interviewed24_48,
  "Number Interviewed within 48 hours"  = interviewed48,
  "Number of Contacts Identified"  = contact_ident,
  "Number of Cases - Refused Interview"  = refused_ident,
  "Number of Cases closed as Unable to Reach" = unable_ident,
  "Total Cases Closed"  = total_cs_clsd
)


excel_data <- readxl::read_excel("V:/Productivity/Daily Report for Administration/test/productivity report.xlsx",
                                 col_types = c("text", "date", rep("numeric", times = 10)))

new_excel_data <- bind_rows(excel_data, row_to_add)


openxlsx::write.xlsx(new_excel_data, file = "V:/Productivity/Daily Report for Administration/test/productivity report.xlsx")
