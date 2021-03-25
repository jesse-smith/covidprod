library(tidyverse)

NCA <- load_nca()
NIT <- load_nit()

library(dplyr)


NCA$assign_date <- lubridate::parse_date_time(NCA$assign_date, orders= "ymdHM")
glimpse(NCA)

NCA$inttime <- stringr::str_sub(NCA$answer_4, 1, 16)

NCA$interviewtime <- lubridate::parse_date_time(NCA$inttime, orders= "ymdHM")
glimpse(NCA)


library(dplyr)


date_asg_int <- subset (NCA, select= c(record_id,assign_date,answer_3,interviewtime))


date_asg_int$timetoint <- difftime(date_asg_int$interviewtime,
                                   date_asg_int$assign_date, units = c("hours"))

interview <- subset(date_asg_int, answer_3=="Yes")

table(NCA$answer_9)
?difftime

assigned<-sum("2021-03-16 23:59:00" > date_asg_int$assign_date &
                date_asg_int$assign_date> "2021-03-16 00:00:00", na.rm = TRUE)

interviewed <- sum(date_asg_int$answer_3== "Yes" & "2021-03-16 23:59:00" > date_asg_int$interviewtime &
                     date_asg_int$interviewtime> "2021-03-16 00:00:00", na.rm = TRUE)
intervcheck <- sum(interview$interviewtime > "2021-03-16 00:00:00" &
                     interview$interviewtime < "2021-03-16 23:59:00", na.rm = TRUE)
int_yest <- subset(interview, interview$interviewtime > "2021-03-16 00:00:00" &
                     interview$interviewtime < "2021-03-16 23:59:00")

glimpse(interview)


interviewed24 <- sum(int_yest$timetoint<= 24, na.rm = TRUE)

interviewed24_48 <- sum(int_yest$timetoint>= 24 & int_yest$timetoint<=48, na.rm = TRUE)

interviewed48 <- sum(int_yest$timetoint<=48, na.rm = TRUE)

percent24 <- (interviewed24/interviewed)


NIT <- load_nit()

#WHY DOES THIS SAY 1 OR MORE PARSING ISSUE??
NIT$dateap <- lubridate::parse_date_time(NIT$date, orders= "ymdHM")

count(NIT, date)
count(NIT, dateap)

glimpse(NIT$dateap)
glimpse(NIT$date)

contacts<- subset(NIT, select= c(dateap, numb_contacts_16))
contacts_yest <- subset (contacts,contacts$dateap > "2021-03-16 00:00:00"
                         & contacts$dateap < "2021-03-16 23:59:00")
contacts_yest$numb_contacts_16<- as.numeric(contacts_yest$numb_contacts_16)
contact_ident<- sum(contacts_yest$numb_contacts_16, na.rm = TRUE)



#all of these are 0 since march 3
unable<- subset(NIT, select= c(resultofinter_3, dateap))
unable_yest <- subset(unable, unable$dateap > "2021-03-16 00:00:00"
                      & unable$dateap < "2021-03-16 23:59:00")
unable_yest$resultofinter_3 <- as.numeric(unable_yest$resultofinter_3)
unable_ident<- sum(unable_yest$resultofinter_3, na.rm = TRUE)


#START HERE

#why are all of these 0 since feb 2?
refused<- subset(NIT, select= c(resultofinter, dateap))
refused_yest <- subset(refused, refused$dateap > "2021-03-16 00:00:00"
                      & unable$dateap < "2021-03-16 23:59:00")
refused_yest$resultofinter <- as.numeric(refused_yest$resultofinter)
refused_ident <- sum (refused_yest$resultofinter, na.rm = TRUE)


total_cs_clsd <- (interviewed+refused_ident+unable_ident)

row_to_add <- tibble(
  Day = weekdays(Sys.Date() - 1L),
  Date = Sys.Date() - 1L
)

excel_data <- readxl::read_excel(col_types = c("text", "date", rep("numeric", times = 9)))

new_excel_data <- bind_rows(excel_data, row_to_add)

#write the file path to overwrite the previous file
openxlsx::write.xlsx(new_excel_data, file = )
