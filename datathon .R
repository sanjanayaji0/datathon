library(tidyverse)
library(lubridate)


## Loading the data set
grocery_store_data <- read.csv("/Users/sanjana/Desktop/datathon/sales_data_2017_2018.csv")

# hour category 
grocery_store_data$hour <- hour(as.POSIXct(grocery_store_data$date, format = "%m/%d/%Y %I:%M:%S %p"))

## 2017 Data
grocery_store_data$date <- as.Date(grocery_store_data$date, format="%m/%d/%Y %I:%M:%S %p")

# Filter the dataset for data in the year 2017
df_2017 <- subset(grocery_store_data, format(grocery_store_data$date, "%Y") == "2017")

# Filter the dataset for data in the year 2018 
df_2018 <- subset(grocery_store_data, format(grocery_store_data$date, "%Y") == "2018")

## Sales volume: Compare the total sales volume for the two years, as well as the volume for 
## individual food products to see if there were any changes in popularity of certain items 
## between the two years. 

total_sales_volume_2018  <- df_2018 %>%
  summarise(total_sales_volume = sum(total_profit))

total_sales_volume_2017 <- df_2017 %>%
  summarise(total_sales_volume = sum(total_profit))
  
price_difference <- total_sales_volume_2017 - total_sales_volume_2018

## Purchasing Habits within the years: time of day, items 


# Count the frequency of each hour in 2017
hourly_counts_2017 <- table(df_2017$hour)
hourly_counts_2017 <- sort(hourly_counts_2017, decreasing = TRUE)
hourly_counts_2017 <- data.frame(hourly_counts_2017) 
hourly_counts_2017$Var1 <- as.numeric(as.character(hourly_counts_2017$Var1))

## hours that aren't 7 am to 7 pm 
hourly_counts_2017_filtered <- hourly_counts_2017[hourly_counts_2017$Var1 < 9, ]
hourly_counts_2017_total <- hourly_counts_2017_filtered %>%
  summarise(total_trans = sum(Freq))
hourly_counts_2017_filtered2 <- hourly_counts_2017[hourly_counts_2017$Var1 > 21, ]
hourly_counts_2017_total2 <- hourly_counts_2017_filtered2  %>%
  summarise(total_trans = sum(Freq))

total_lost <- hourly_counts_2017_total + hourly_counts_2017_total2

# Count the frequency of each hour in 2018 
hourly_counts_2018 <- table(df_2018$hour)
hourly_counts_2018 <- sort(hourly_counts_2018, decreasing = TRUE) 

hourly_counts_2018 <- data.frame(hourly_counts_2018) 

sum <- sum(hourly_counts_2018$Freq)



## most likely time to open: 0-5, 11-18


## basket analysis: 
library(dplyr)

# Assuming your data frame is called "sales_data" and the column with the receipt IDs is called "receipt_id"

# Group the data by receipt ID and create a new column "item_list" containing the items bought in each receipt
basket_data <- grocery_store_data %>% 
  group_by(receipt_id) %>% summarize(item_list = paste(item_name, collapse = ", "))

# Convert the item_list column to a list format for use with the arules package
basket_data_list <- strsplit(basket_data$item_list, split = ", ")

library(arules)

# Create a transaction object
trans <- as(basket_data_list, "transactions")

# Generate association rules
rules <- apriori(trans, parameter = list(supp = 0.001, conf = 0.5))

# Inspect the rules
inspect(rules)

# Sort the rules by lift
rules_sorted <- sort(rules, by = "lift")

# Extract the antecedent and consequent items from the top 10 rules
top_rules <- head(rules_sorted, n = 10)
antecedent <- labels(lhs(top_rules))
consequent <- labels(rhs(top_rules))

# Print the top 10 rules and their antecedent and consequent items
for (i in 1:length(top_rules)) {
  cat("\nRule ", i, ":\n")
  cat(antecedent[[i]], " => ", consequent[[i]], "\n")
}

library(dplyr)

# count the number of times each item was sold
item_counts <- grocery_store_data %>% count(item_name)

# sort the items by the number of times they were sold, in ascending order
item_counts_sorted <- item_counts %>% arrange(n)

# select the item that was sold the least
least_sold_item <- item_counts_sorted[1, "item_name"]

least_sold_transactions <- grocery_store_data %>% filter(item_name == least_sold_item)
total_revenue_least_sold <- sum(least_sold_transactions$total_selling_price)

library(dplyr)

# count the number of times each item was sold and sort them in ascending order
item_counts1 <- grocery_store_data %>% count(item_name) %>% arrange(n)

# select the top 10 least sold items
top_10_least_sold_items <- item_counts1 %>% slice_head(n = 20)


library(dplyr)

# count the number of times each item was sold and sort them in descending order
item_counts <- grocery_store_data %>% count(item_name) %>% arrange(desc(n))

# select the top 10 most sold items
top_10_most_sold_items <- item_counts %>% slice_head(n = 10)

# Load the ggplot2 package for plotting
library(ggplot2)




