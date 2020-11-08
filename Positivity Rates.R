
library(openxlsx)
library(tidyverse)


sample_negative <- read.xlsx("negativedata.xlsx")

by_date_negative <- "sample_negative" %>% 
    filter("result" == "negative") %>% 
    group_by("date_teted") %>% 
    summarise_all(`Number of residents Negative` = 'n()') %>% 
    arrange("date_tested")




sample_positive <- read.xlsx("positivedata.xlsx")

by_date_positive <- "sample_positive" %>% 
    filter("result" == "positive") %>% 
    group_by("date_teted") %>% 
    summarise_all(`Number of residents positive` = 'n()') %>% 
    arrange("date_tested")


# merge the two data-frames two calculate the total resident tested and the positivity-rate and rolling average of 
# each day


merge_data <- by_date_negative %>% 
    full_join(by_date_negative, by = 'date_tested') %>% 
    mutate(`Total Resident Tested` = `Number of residents positive` + `Number of residents Negative`,
           `Positivity Rate`= `Number of residents Negative`/`Total Resident Tested` * 100,
           `Tests per dat Rolling average` = (`Total Resident Tested` - lag(`Total Resident Tested`))/lag(`Total Resident Tested`))

overall_positivity <- merge_data %>% 
    summarise(`Total Person Negative` = sum(`Number of residents Negative`),
              `Total Person Positive` = sum(`Number of residents positive`),
              `Total Resident Tested` = sum(`Total Resident Tested`),
              `Positivity Rate` = `Total Person Positive` / `Total Resident Tested` / 100) %>% 
    mutate(`As of` = max("date_tested"))

twoweek_positivity <-  merge_data %>%
    filter("date_tested" >= "2020-10-18" & "date_tested" <= "2020-10-31") %>% 
    summarise(`Total Person Negative` = sum(`Number of residents Negative`),
              `Total Person Positive` = sum(`Number of residents positive`),
              `Total Resident Tested` = sum(`Total Resident Tested`),
              `Positivity Rate` = `Total Person Positive` / `Total Resident Tested` / 100) %>% 
    mutate(`As of` = paste('Oct 18 - Oct 31'))




# writing data frame to excel files

list_tables <- list(merge_data, overall_positivity, twoweek_positivity)

names(list_tables) <- c("By Sample Date", "Overall Positivity" , "TwoWeek Positivity")
write.xlsx(list_tables, "Positivity Rates.xlsx")





















    

 