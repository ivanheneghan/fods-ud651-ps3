# Install packages
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("lubridate")

# Load relevant libraries
library(data.table)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(plyr)
library(reshape2)
library(tidyr)
library(xlsx)

# Load diamonds data set, and initial analysis
str(diamonds)
head(diamonds)
colnames(diamonds)
summary(diamonds)
View(diamonds)

# Histogram of price of all the diamonds
qplot(x = price, data = diamonds, binwidth = 10)

# Summary of price information
summary(diamonds$price)

# Counts
sum(diamonds$price < 500)
sum(diamonds$price < 250)
sum(diamonds$price >= 15000)

# Exploring the largest peak in the histogram
# Entire histogram
qplot(x = price, data = diamonds, binwidth = 250, 
      xlab = "Price", ylab = "Count of diamonds") +
  scale_x_continuous(limits = c(250, 20000), breaks = seq(250, 20000, 2500))
# Zooming in on highest peak
qplot(x = price, data = diamonds, binwidth = 50, 
      xlab = "Price", ylab = "Count of diamonds") +
  scale_x_continuous(limits = c(250, 2000), breaks = seq(250, 2000, 250))

# Break out prices by cut
# Histograms
qplot(x = price, data = diamonds, binwidth = 250, 
      xlab = "Price", ylab = "Count of diamonds") +
  scale_x_continuous(limits = c(250, 20000), breaks = seq(250, 20000, 2500)) + 
  facet_wrap(~cut)
# Numerical
by(diamonds$price, diamonds$cut, summary)
# Using Max due to rounding
by(diamonds$price, diamonds$cut, max)

# Scales & multiple histograms
qplot(x = price, data = diamonds, binwidth = 10) + 
  facet_wrap(~cut, scales = "free_y")

# Price per Carat by Cut
qplot(x = price/carat, data = diamonds, binwidth = 50) +
  facet_wrap(~cut, scales = "free_y") +
  scale_x_log10()

# Box plot - Price of diamonds, numerical summaries, and one other variable
qplot(x = color, y = price, 
      data = diamonds, 
      geom = "boxplot",
      color = color) +
  coord_cartesian(ylim = c(250, 20000))

# IQR analysis
by(diamonds$price, diamonds$color, summary)
IQR(subset(diamonds, color == "D" )$price)
IQR(subset(diamonds, color == "J")$price)

# Box plot - Price per carat of diamonds across colors
qplot(x = color, y = price/carat , 
      data = diamonds, 
      geom = "boxplot",
      xlab = "Color",
      ylab = "Price per Carat",
      color = color) +
  coord_cartesian(ylim = c(250, 20000))

# Frequency polygon
qplot(x = carat, data = diamonds, 
      binwidth = 0.05,
      xlab = "Carat",
      ylab = "Count",
      geom = "freqpoly") +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 10, 0.1))

# Gapminder analysis - working hours per week

# Import data
whpw <- read.xlsx("D:\\Foundations of Data Science\\Projects\\EDA - Facebook\\fods-ud651-ps3\\indicator_hours per week.xlsx", 1, stringsAsFactors = FALSE)

# Rename first column to something more meaningful
names(whpw)[1] <- c("country")

# Tidy data - gather into two column key-value pair
whpw <- whpw %>% gather(year, hours, -country)

# Remove X's from start of years
whpw$year <- gsub("X", "", whpw$year)

# Create data table from the imported data
tbl_whpw <- tbl_df(whpw)

# Initial viewing of data
str(tbl_whpw)
head(tbl_whpw)
colnames(tbl_whpw)
summary(tbl_whpw)
View(tbl_whpw)

# Boxplot showing progression over time
ggplot(tbl_whpw[!is.na(whpw$hours),], aes(x = year, y = hours, fill = year)) + 
  geom_boxplot() +
  xlab("Year") +
  ylab("Working hours per week") +
  ggtitle("Working hours per week over time")

# Line graph
ggplot(aes(x = year, y = hours, group = country, color = country), data = tbl_whpw, binwidth = 5) +
  geom_line() +
  xlab("Year") +
  ylab("Working hours per week") +
  ggtitle("Working hours per week over time")

# Subsetting to look at countries with > 40 hours per week over entire time period
ggplot(aes(x = year, y = hours, group = country, color = country), data = subset(tbl_whpw, hours > 40) , binwidth = 5) +
  geom_line() +
  xlab("Year") +
  ylab("Working hours per week") +
  ggtitle("Working hours per week over time")

# Subsetting to look at countries with < 40 hours per week over entire time period
ggplot(aes(x = year, y = hours, group = country, color = country), data = subset(tbl_whpw, hours < 40) , binwidth = 5) +
  geom_line() +
  xlab("Year") +
  ylab("Working hours per week") +
  ggtitle("Working hours per week over time")

# Melting data to allow delta change calculation & histogram
tbl_whpw <- spread(tbl_whpw, year, hours)

# Getting delta change for each country between 1990 and 2007
tbl_whpw$delta <- tbl_whpw$`2007` - tbl_whpw$`1990`

# Reviewing sumary of delta change
summary(tbl_whpw$delta)

# Looking at histograms for 1990 and 2007
g1 <- ggplot(aes(tbl_whpw$`1990`), data = tbl_whpw, binwidth = 1) + 
  geom_histogram() +
  xlab("Working hours per week") +
  ylab("Count of countries") +
  ggtitle("Working hours in 1990")

g2 <- ggplot(aes(tbl_whpw$`2007`), data = tbl_whpw, binwidth = 1) + 
  geom_histogram() +
  xlab("Working hours per week") +
  ylab("Count of countries") +
  ggtitle("Working hours in 2007")

grid.arrange(g1, g2, ncol = 1)

# Exploring friend's birthdays

# Import data
birthdays <- read.csv("D:\\Foundations of Data Science\\Projects\\EDA - Facebook\\fods-ud651-ps3\\birthdaysExample.csv", stringsAsFactors = F)

# Tidy data
birthdays$betterDates <- as.Date(birthdays$dates, format = "%m/%d/%y")

# Extract year, month, day, day of year
birthdays <- birthdays %>% mutate(year = year(birthdays$betterDates), month = month(birthdays$betterDates), day = day(birthdays$betterDates))
birthdays <- birthdays %>% mutate(dayOfYear = yday(birthdays$betterDates))

# Viewing of data
str(birthdays)
head(birthdays)
colnames(birthdays)
summary(birthdays)
View(birthdays)

# Number of people sharing my birthday (March 26th) - 2 people
count(subset(birthdays, month == "3" & day == "26"))

# Which month has the most birthdays? - March
ggplot(data = birthdays, aes(x = month)) + 
    geom_histogram(binwidth = 1, color = "white", fill = "dark blue") +
    xlab("Month") +
    ylab("Count of Birthdays") +
    ggtitle("Birthdays per Month") +
  scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, 1))

# Number of birthdays in each month
count(birthdays$month)

ggplot(data = birthdays, aes(x = month)) + 
  geom_histogram(binwidth = 1, color = "white", fill = "dark blue") +
  xlab("Month") +
  ylab("Count of Birthdays") +
  ggtitle("Birthdays per Month") +
  scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, 1)) +
  scale_y_continuous(breaks=seq(60, 100, 1)) +
  coord_cartesian(ylim=60:100)

# Day of the year with most birthdays - 37th, 142nd and 197th days (Feb 6th, May 22nd and July 16th)
birthdaysPerDay <- count(birthdays$dayOfYear)
birthdaysPerDay <- arrange(birthdaysPerDay, desc(freq))
head(birthdaysPerDay)

ggplot(data = birthdays, aes(x = day)) + 
  geom_histogram(binwidth = 1, color = "white", fill = "dark blue") +
  xlab("Months") +
  ylab("Count of Birthdays") +
  ggtitle("Birthdays per Day, per Month") +
  scale_x_continuous(limits = c(1, 31), breaks = seq(1, 31, 1)) +
  facet_wrap(~month, ncol = 3)

# Friends with birthdays every day of the year? - No, as there are gaps in the graph
ggplot(data = birthdays, aes(x = dayOfYear)) + 
  geom_histogram(binwidth = 1, color = "white", fill = "dark blue") +
  xlab("Day") +
  ylab("Count of Birthdays") +
  ggtitle("Birthdays per Day") +
  scale_x_continuous(limits = c(1, 366), breaks = seq(1, 366, 1))