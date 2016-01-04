# Install packages
install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

# Load diamonds data set, and initial analysis
str(diamonds)
head(diamonds)
colnames(diamonds)
summary(diamonds)
#View(diamonds)

# Histogram of price of all the diamonds
qplot(x = price, data = diamonds)

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
qplot(x = price, data = diamonds) + 
  facet_wrap(~cut, scales = "free_y")

# Price per Carat by Cut
qplot(x = price, data = diamonds, binwidth = 250, 
      xlab = "Price", ylab = "Count of diamonds") +
  scale_x_continuous(limits = c(250, 20000), breaks = seq(250, 20000, 2500)) + 
  facet_wrap(~cut) +
  scale_x_log10()
