# Title Apple Stock historical data 
# Name: Abdullah Alreaan
# Date: Oct25,2020
# Description: this dataset shown a detailed Apple company stock prices for the past 10 years.

# Describe the research question you are addressing in this analysis
#What was the change in price of the stock over time?
#What was the daily return of the stock on average?
#How can we attempt to predict future stock behavior?

# load packages
install.packages("viridis")
library(viridis)
library(dplyr)
library(hrbrthemes)
library(tidyverse)
library(lubridate)
library(data.table)

# Import data
apple <- read_csv("HistoricalQuotes.csv")

# Data munging
# Clean up the data - 
# Make sure our $ columns are actually numeric

glimpse(apple)
str(apple)
sapply(apple, mode)
sum(is.na(apple))

apple$Open = as.numeric(gsub("\\$", "", apple$Open))
apple$`Close/Last` = as.numeric(gsub("\\$", "", apple$`Close/Last`))
apple$High = as.numeric(gsub("\\$", "", apple$High))
apple$Low = as.numeric(gsub("\\$", "", apple$Low))

# Make sure the date is actually a date

apple$Date = as.Date(apple$Date, format = "%m/%d/%Y")

strptime(apple$Date, "%m/%d/%Y")


# explore data:
summary(apple)

# average share price Open for each of the last 10 yrs
apple %>% 
  group_by(Date) %>% 
  summarise(mean_Open = mean(Open))



# calculate the year-on-year percentage change of price:

setDT(apple_summary)[, yoy_per := MEAN/shift(MEAN) -1]

ggplot(apple_summary, aes(year, new.col, group = 1)) +
  geom_point() +
  geom_line() +
  labs(x = "YEAR", 
       y = "percentage", 
       title = "year-on-year percentage increase")



# Global average share price in the last 10 yrs
apple %>%
  summarise(mean_Open = mean(Open))

apple %>% 
  summarise(mean_of_Volume = mean(Volume),
            mean_of_closing = mean(`Close/Last`))


# 2020 average share price. 
apple %>%
  # mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  mutate (year = format(Date, "%Y")) %>% 
  group_by(year == 2020) %>%
  summarise(MEAN = mean(`Close/Last`))


# plot1
ggplot(apple_summary, aes(MEAN, year, group = 1)) +
  geom_point() +
  geom_line() +
  labs(x = "PRICE", 
       y = "YEAR", 
       title = "AVG APPL STOCK PRICE")

# plot2
ggplot(apple_summary, aes(year, MEAN, fill = year)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "YEAR",
       y = "PRICE",
       title = paste("APPL AVG STOCK PRICE IN LAST 10 YRS"))


