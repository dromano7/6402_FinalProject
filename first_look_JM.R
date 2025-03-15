library(lubridate)
library(tseries)
library(dplyr)
library(scales)
library(forecast)

# read in
homeownership_rate_data <- read.csv("data/Homeownership Rate in the United States.csv", head=TRUE, stringsAsFactors =  FALSE)
personal_income_data <- read.csv("data/Real Disposable Personal Income.csv", head=TRUE)
HPI_data <- read.csv("data/S&P CoreLogic Case-Shiller U.S. National Home Price Index.csv", head=TRUE)

######### HOMEOWNERSHIP RATE ######### 
# format
homeownership_rate_data <- homeownership_rate_data %>% 
  mutate(observation_date = as.Date(observation_date, format = "%Y-%m-%d"))
homeownership_rate_data <- homeownership_rate_data %>%
  rename(
    value = RHORUSQ156N
  )
# convert to ts and plot
hr_ts <- ts(homeownership_rate_data$value, start=c(1987,1), freq=4)
plot(hr_ts, yaxt="n", xlab= "Year", ylab="", main="Homeownership Rate in the U.S.")
axis(2, at = pretty(hr_ts), labels = paste0(pretty(hr_ts), "%"), las = 2)
# decompose
hr_decomp <- decompose(hr_ts)
plot(hr_decomp)

# trend and seasonality both present. The housing crisis (2008) seems to be the peak rate for this period. 
# trend is positive, then negative. seasonality is a simple spike pattern and appears to be yearly. 
# randomness looks to be white noise except for increase in variance during COVID (2020).

# plot the acf
acf(hr_ts, lag.max = 200, main="")
pacf(hr_ts, lag.max = 200, main="")
# acf is sinusoidal
# seasonality present in the pacf


######### PERSONAL INCOME ######### 
# format
personal_income_data <- personal_income_data %>% 
  mutate(observation_date = as.Date(observation_date, format = "%Y-%m-%d"))
personal_income_data <- personal_income_data %>%
  rename(
    value = DSPIC96
  )
# convert to ts and plot
pi_ts <- ts(personal_income_data$value, start=c(1987,1), freq=12)
plot(pi_ts, yaxt="n", ylab="", xlab= "Year", main="Real Disposable Personal Income")
axis(2, at = pretty(pi_ts), labels = dollar(pretty(pi_ts)), las = 2)
# decompose
pi_decomp <- decompose(pi_ts)
plot(pi_decomp)

# trend and seasonality both present. In this data set, the trend is positive throughout the entire horizon.
# despite the fact that this is SAAR data and some seasonality is already adjusted for, there still seems to be
# some left in the data, and the "randomness" also has a level of cyclicality present.  

# plot the acf
acf(pi_ts, lag.max = 200, main="")
pacf(pi_ts, lag.max = 200, main="")
# acf is linear
# pacf shows seasonality


######### HPI ######### 
# format
HPI_data <- HPI_data %>% 
  mutate(observation_date = as.Date(observation_date, format = "%Y-%m-%d"))
HPI_data <- HPI_data %>%
  rename(
    value = CSUSHPINSA
  )
# convert to ts and plot
hpi_ts <- ts(HPI_data$value, start=c(1987,1), freq=12)
plot(hpi_ts, ylab="Index Jan. 2000 = 100", xlab= "Year", main="Home Price Index")
# decompose
pi_decomp <- decompose(pi_ts)
plot(pi_decomp)

# not seasonally adjusted
# values above 100 indicate a % increase from Jan. 2000 base
# overall positive trend but negative in specific window

# plot the acf
acf(pi_ts, lag.max = 200, main="")
pacf(pi_ts, lag.max = 200, main="")
# acf is linear
# pacf shows seasonality



rm(list = ls())
