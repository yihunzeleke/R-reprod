library(dplyr)
library(dslabs)

data("heights")

s <- heights %>% 
  filter(sex == "Male") %>% 
  summarise(average = mean(height), standar_deviation = sd(height))

# getting quantile in tidying form
heights %>% 
  filter(sex == "Male") %>% 
  summarise(range = quantile(height, c(0,0.5,1)))

## dot placeholder 

data("murders")

murders <- murders %>% 
  mutate(murder_rate = total/population*100000)

## we noticed that the US murder rate is not the average of  states
murders %>% 
  summarise(average_rate = mean(total))

## The correct computation
murders %>% 
  summarise(average_rate = sum(total)/sum(population)* 100000)  # output is data frame

# placeholder 

murders %>% 
  summarise(rate = sum(total)/ sum(population) * 100000) %>% # returns numeric values not data frame
  
  .$rate

pl <- murders %>% 
  summarise(rate = sum(total)/ sum(population) * 100000) %>% # returns numeric values not data frame
  pull()

str(pl)

## group_by()

murders %>% 
  group_by(region) %>% 
  summarise(median_rate = median(murder_rate)) 
















