library(zoo)
library(ggplot2)
library(forecast)
library(Metrics)

joba <- read.csv('PCU721110721110.csv')

names(joba)[1] <- 'Date'
names(joba)[2] <- 'Value'

Sys.setlocale("LC_TIME", "English_United States")
joba$Date <- as.yearmon(joba$Date)


test_set_end <- as.yearmon("Jan 2022")
test_set_start <- as.yearmon("Feb 2020")

training_set <- subset(joba, Date < test_set_start)
test_set <- subset(joba, Date > test_set_end)


ggplot() +
  geom_line(data = joba, aes(x = Date, y = Value, color = "Covid"), size = 1) +
  geom_line(data = test_set, aes(x = Date, y = Value, color = "Testing"), size = 1) +
  geom_line(data = training_set, aes(x = Date, y = Value, color = "Training"), size = 1) +
  labs(
    title = "Hotels and Motels - Training and Testing Sets",
    x = "Date",
    y = "Producer Price Index by Industry"
  ) +
  scale_color_manual(values = c("Covid" = "#12355B", "Testing" = "#D72638", "Training" = "#CFAE00"), name = "Index") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))



#Dealing with sub data


cut_subdata_start <- as.yearmon("Jan 2012")
cut_subdata_end <- as.yearmon("Mar 2020")

subdata_set <- subset(training_set, Date > cut_subdata_start & Date < cut_subdata_end)

plot(subdata_set, type="l")

ggplot(data = subdata_set, aes(x = Date, y = Value)) +
  geom_line(color = "#CFAE00", size = 1) +
  labs(
    title = "Subdata Set: Hotels and Motels (2012 - 2020)",
    subtitle = "Analysis of the period before the COVID-19 impact",
    x = "Year",
    y = "Producer Price Index"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 12)
  )




subdata_set_ts <- ts(subdata_set$Value, frequency = 12)

autoplot(subdata_set_ts, color = "#12355B") +
  labs(title="Original Time Series",
       x="Year",
       y="Value") +
  theme_minimal()




log_subdata_ts <- log(subdata_set_ts)

autoplot(log_subdata_ts, color = "#12355B") +
  labs(title="Log-transformed Series",
       x="Year",
       y="log(Value)") +
  theme_minimal()

acf(log_subdata_ts, lag.max = 48)




diff_subdata_ts <- diff(log_subdata_ts)

diff_subdata_ts <- ts(diff_subdata_ts, frequency = 12)
autoplot(diff_subdata_ts, color = "#12355B") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(
    title = "First-Order Differencing (Non-seasonal)",
    subtitle = "Removing trend to achieve mean stationarity",
    x = "Year",
    y = "dLog(Value)"
  ) +
  theme_minimal()

acf(diff_subdata_ts, lag.max = 48)



seasonal_diff_subdata_ts <- diff(diff_subdata_ts,12)
seasonal_diff_subdata_ts <- ts(seasonal_diff_subdata_ts, frequency = 12)

autoplot(seasonal_diff_subdata_ts, color = "#12355B") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Second-Order Differencing (Seasonal)",
    subtitle = "Removing trend to achieve mean stationarity",
    x = "Year",
    y = "dLog(Value)"
  ) +
  theme_minimal() #White Noise for ARIMA analysis

acf(seasonal_diff_subdata_ts, lag.max = 48, drop.lag.0 = F)
pacf(seasonal_diff_subdata_ts, lag.max = 48, drop.lag.0 = F)


acf(subdata_set_ts, lag.max = 48)
pacf(subdata_set_ts, lag.max = 48)
