# Converting character columns to factor
df_master %>% 
  mutate_if(is.character, as.factor)


## ----------------------------------------------------------------------------------------------
# Conditional Statements in R
ctr = 1
while(ctr <= 7){
  print(paste("ctr is set to", ctr))
  ctr <- ctr + 1
}

# while loop with the beak statement
ctr = 1
while(ctr <= 7)
{
  if (ctr %%5 == 0){
    break
  }
  print(paste("ctr is set to", ctr))
  ctr <- ctr + 1
}


# Initialize the speed variable
speed <- 88

while (speed > 30) {
  print(paste("Your speed is", speed))
  
  # Break the while loop when speed exceeds 80
  if (speed > 80 ) {
    break
  }
  
  if (speed > 48) {
    print("Slow down big time!")
    speed <- speed - 11
  } else {
    print("Slow down!")
    speed <- speed - 6
  }
}

## ----------------------------------------------------------------------------------------------
# Functions
# The linkedin and facebook vectors have already been created for you
linkedin <- c(16, 9, 13, 5, 2, 17, 14)
facebook <- c(17, 7, 5, 16, 8, 13, 14)

# The interpret() can be used inside interpret_all()
interpret <- function(num_views) {
  if (num_views > 15) {
    print("You're popular!")
    return(num_views)
  } else {
    print("Try to be more visible!")
    return(0)
  }
}

# Define the interpret_all() function
# views: vector with data to interpret
# return_sum: return total number of views on popular days?
interpret_all <- function(views, return_sum = TRUE) {
  count <- 0
  
  for (v in views) {
    count <- count + interpret(v)
  }
  
  if (return_sum) {
    return(count)
  } else {
    return(NULL)
  }
}

# Call the interpret_all() function on both linkedin and facebook
interpret_all(linkedin)
interpret_all(facebook)

## ----------------------------------------------------------------------------------------------
# Definition of split_low
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")
split <- strsplit(pioneers, split = ":")
split_low <- lapply(split, tolower)

# Generic select function
select_el <- function(x, index) {
  x[index]
}

# Use lapply() twice on split_low: names and years
names <- lapply(split_low, select_el, index = 1)
years <- lapply(split_low, select_el, index =2)

## ----------------------------------------------------------------------------------------------
# The readxl package is already loaded
library(readxl)
# Read all Excel sheets with lapply(): pop_list
pop_list <- lapply(excel_sheets("urbanpop.xlsx"),read_excel, path = "urbanpop.xlsx")s

# Display the structure of pop_list
str(pop_list)
## ----------------------------------------------------------------------------------------------
# Real time MySQL connection with RStudion
#install.packages('RMySQL')
library(RMySQL)
library(DBI)

con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "tweater", 
                 host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                 port = 3306,
                 user = "student",
                 password = "datacamp")
dbListTables(con) # List the tables in the server
dbReadTable(con, "users") # Importing the data from the server

# Get table names
table_names <- dbListTables(con)

# Import all tables
tables <- lapply(table_names, dbReadTable,conn = con)

# Print out tables
tables

# Import tweat_id column of comments where user_id is 1: elisabeth
elisabeth <- dbGetQuery(con, "SELECT  tweat_id FROM comments
WHERE user_id = 1")

# Create data frame short

short <- dbGetQuery(con, "SELECT id,name FROM users
WHERE CHAR_LENGTH(name) < 5")

dbGetQuery(con, "SELECT post, message FROM tweats INNER JOIN comments on tweats.id = tweat_id
    WHERE tweat_id = 77")
# Send query to the database
res <- dbSendQuery(con, "SELECT * FROM comments WHERE user_id > 4")

# Use dbFetch() twice
# Use dbFetch() twice
dbFetch(res, n = 2)
dbFetch(res)

# Clear res
dbClearResult(res)

dbDisconnect(con) # Disconnecting the server


