library(openxlsx)
library(tidyverse)
library(lubridate)

# Importing and Aggregating Data -----------------------------------------------------------------------------------

bc <- read_csv("data/KCMO_Business_Complaints.csv") %>% 
  janitor::clean_names() 

bc <- bc %>% 
  select(source, creation_date, status, zip_code, county, council_district,police_district) %>% 
  mutate(creation_date = lubridate::mdy(creation_date))

complaint_source <- bc %>% 
  group_by(source) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))


complaint_status <- bc %>% 
  group_by(status) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

complaints_day <- bc %>% 
  mutate(day = wday(creation_date, label = T)) %>% 
  group_by(day) %>% 
  summarise(count_by_day = n())

complaint_month <- bc %>% 
  mutate(month = month(creation_date, label = T)) %>% 
  group_by(month) %>% 
  summarise(count_by_month = n())

complaint_county <- bc %>% 
  group_by(county) %>% 
  summarise(county_count = n())


compliant_county_district <- bc %>% 
  group_by(council_district) %>% 
  summarise(district_count = n())

complaint_zipcode <- bc %>% 
  group_by(zip_code) %>% 
  summarise(counts_zipcode = n()) %>% 
  arrange(desc(counts_zipcode))

complaint_police_district <- bc %>% 
  group_by(police_district) %>% 
  summarise(counts_police_district = n())


# Pulled Out to summary table --------------------------------------------------------------------------------------


summary_table <- createWorkbook()

# Add some sheets to the workbook
addWorksheet(summary_table, "complaint_source")
addWorksheet(summary_table, "complaint_status")
addWorksheet(summary_table, "complaints_day")
addWorksheet(summary_table, "complaint_month")
addWorksheet(summary_table, "complaint_county")
addWorksheet(summary_table, "compliant_county_district")
addWorksheet(summary_table, "compliant_zipcode")
addWorksheet(summary_table, "complaint_police_district")


# Write the data to the sheets
writeData(summary_table, "complaint_source", complaint_source)
writeData(summary_table, "complaint_status", complaint_status)
writeData(summary_table, "complaints_day", complaints_day)
writeData(summary_table, "complaint_month", complaint_month)
writeData(summary_table, "complaint_county", complaint_county)
writeData(summary_table, "compliant_county_district", compliant_county_district)
writeData(summary_table, "compliant_zipcode", complaint_zipcode)
writeData(summary_table, "complaint_police_district", complaint_police_district)

# Export the file
saveWorkbook(summary_table, "summary_table.xlsx")







