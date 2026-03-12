#LIBRARIES

library(zoo)
library(ggplot2)
library(forecast)
library(Metrics)

###############################################                         
###############################################
####                                       ####
####       SIMULATING OUR MAIN DATA        ####
####                                       ####
###############################################
###############################################

joba <- read.csv('PCU721110721110 (2).csv')

names(joba)[1] <- 'Date'
names(joba)[2] <- 'Value'


joba$Date <- as.yearmon(joba$Date)


test_set_end <- as.yearmon("May 2020")
test_set_start <- as.yearmon("Feb 2020")

training_set <- subset(joba, Date < test_set_start)
test_set <- subset(joba, Date < test_set_start) #as.yearmon("Oct 2019")


plot(joba, type = 'l')


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




###############################################                         
###############################################
####                                       ####
####         WORKING WITH A SUBSET         ####
####         (PRILIMINARY ANALYSIS)        ####
####                                       ####
###############################################
###############################################



cut_subdata_start <- as.yearmon("Jan 2012")

subdata_set <- subset(training_set, Date > cut_subdata_start)



subdata_set_ts <- ts(subdata_set$Value, frequency = 12)
diff_subdata_set_ts <- diff(subdata_set_ts)
season_diff_subdata_set_ts <- diff(diff_subdata_set_ts, 12)

acf(season_diff_subdata_set_ts, lag.max = 48)
pacf(season_diff_subdata_set_ts, lag.max = 48)



###############################################                         
###############################################
####                                       ####
####          FORECASTING SUBSET           ####
####      (TRAININ AND TEST SUBSETS)       ####
####                                       ####
###############################################
###############################################

cut_subdata_end <- as.yearmon("Dec 2018")

training_set <- subset(subdata_set, Date < cut_subdata_end)
test_set <- subset(subdata_set, Date >= cut_subdata_end)

ts_training_set <- ts(training_set$Value, frequency = 12)


ggplot() +
  geom_line(data = test_set, aes(x = Date, y = Value, color = "Testing"), size = 1) +
  geom_line(data = training_set, aes(x = Date, y = Value, color = "Training"), size = 1) +
  labs(
    title = "Hotels and Motels - Training and Testing Sets",
    x = "Date",
    y = "Producer Price Index by Industry"
  ) +
  scale_color_manual(values = c("Testing" = "#D72638", "Training" = "#000ADE"), name = "Index") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))


#ma3 <- Arima(ts_training_set, order = c(12,2,5), seasonal = TRUE)

#ma3_forecast <- forecast(ma3, h = nrow(test_set)) 

#Maframe <- data.frame(
#  Date = test_set$Date,
#  MA3 = ma3_forecast$mean)




ggplot() +
  geom_line(data = test_set, aes(x = Date, y = Value, color = "Testing"), size = 1) +
  geom_line(data = training_set, aes(x = Date, y = Value, color = "Training"), size = 1) +
  geom_line(data = Maframe, aes(x = Date, y = MA3, color = "Ma6"), size = 1) +
  labs(
    title = "Hotels and Motels - Training and Testing Sets",
    x = "Date",
    y = "Producer Price Index by Industry"
  ) +
  scale_color_manual(values = c("Testing" = "#D72638", "Training" = "#000ADE", "Ma6" = "#D6B304"), name = "Index") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))




###############################################                         
###############################################
####                                       ####
####          FORECASTING SUBSET           ####
####   (EXPONENTIAL SMOOTHING ESTIMATES)   ####
####                                       ####
###############################################
###############################################


ses <- HoltWinters(ts_training_set, beta = FALSE, gamma = FALSE)
des <- HoltWinters(ts_training_set, gamma = FALSE)
tes <- HoltWinters(ts_training_set)


ses_forecast <- forecast(ses, h = nrow(test_set))
des_forecast <- forecast(des, h = nrow(test_set))
tes_forecast <- forecast(tes, h = nrow(test_set))



exp_forecasting <- data.frame(
  Date = test_set$Date,
  SES = ses_forecast$mean,
  DES = des_forecast$mean,
  TES = tes_forecast$mean
)


ggplot() +
  geom_line(data = test_set, aes(x = Date, y = Value, color = "Testing"), size = 1) +
  geom_line(data = training_set, aes(x = Date, y = Value, color = "Training"), size = 1) +
  geom_line(data = exp_forecasting, aes(x = Date, y = SES, color = "SES"), size = 1) +
  geom_line(data = exp_forecasting, aes(x = Date, y = DES, color = "DES"), size = 1) +
  geom_line(data = exp_forecasting, aes(x = Date, y = TES, color = "TES"), size = 1) +
  labs(
    title = "Hotels and Motels - Training and Testing Sets",
    x = "Date",
    y = "Producer Price Index by Industry"
  ) +
  scale_color_manual(values = c("Testing" = "#D72638", "Training" = "#000ADE",
                                "TES" = "#85D602", "DES" = "#D67A02", "SES" = "#D6BA02"), name = "Index") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))



###############################################                         
###############################################
####                                       ####
####     SIMULATION ON THE MAIN DATA       ####
####                                       ####
###############################################
###############################################

sub_after <- subset(training_set, Date <= cut_subdata_start)
ts_subdata_set <- ts(subdata_set$Value, frequency = 12)




tes_full <- HoltWinters(ts_subdata_set)
tes_full_forecast <- forecast(tes_full, h = nrow(test_set))

final_forecast <- data.frame(
  Date = test_set$Date,
  TES = tes_full_forecast$mean
)


ggplot() +
  geom_line(data = joba, aes(x = Date, y = Value, color = "Data"), size = 1) +
  geom_line(data = final_forecast, aes(x = Date, y = TES, color = "TES"), size = 1) +
  labs(
    title = "Hotels and Motels - Training and Testing Sets",
    x = "Date",
    y = "Producer Price Index by Industry"
  ) +
  scale_color_manual(values = c("Data" = "#000ADE","TES" = "#D72638"), name = "Index") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))











