#LIBRARIES HUUUUY

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

frame_data <- read.csv('PCU721110721110 (2).csv')

names(frame_data)[1] <- 'Date'
names(frame_data)[2] <- 'Value'


frame_data$Date <- as.yearmon(joba$Date)


covid_set_end <- as.yearmon("May 2020")
covid_set_start <- as.yearmon("Feb 2020")

before_lockdown <- subset(frame_data, Date < covid_set_start)
after_lockdown <- subset(frame_data, Date > covid_set_end)


plot(frame_data, type = 'l')


ggplot() +
  geom_line(data = frame_data, aes(x = Date, y = Value, color = "Lockdown"), size = 1) +
  geom_line(data = before_lockdown, aes(x = Date, y = Value, color = "Before Lockdown"), size = 1) +
  geom_line(data = after_lockdown, aes(x = Date, y = Value, color = "After Lockdown"), size = 1) +
  labs(
    title = "Hotels and Motels",
    x = "Date",
    y = "Producer Price Index by Industry"
  ) +
  scale_color_manual(values = c("Lockdown" = "#0006BF", "Before Lockdown" = "#D6B304", "After Lockdown" = "#D17600"), name = "Period") +
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

subdata_set <- subset(before_lockdown, Date > cut_subdata_start)



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
  scale_color_manual(values = c("Testing" = "#D72638", "Training" = "#07BCE0",
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

without_lockdown <- subset(frame_data, Date > as.yearmon("Oct 2019"))


sub_after <- subset(without_lockdown, Date <= cut_subdata_start)
ts_subdata_set <- ts(subdata_set$Value, frequency = 12)




tes_full <- HoltWinters(ts_subdata_set)
tes_full_forecast <- forecast(tes_full, h = nrow(without_lockdown))

final_forecast <- data.frame(
  Date = without_lockdown$Date,
  TES = tes_full_forecast$mean,
  lower95 = tes_full_forecast$lower,
  upper95 = tes_full_forecast$upper
  )


ggplot() +
  geom_line(data = frame_data, aes(x = Date, y = Value, color = "Data"), size = 1) +
  geom_ribbon(data = final_forecast, aes(x = Date, ymin=lower95.95., ymax = upper95.95.),
              fill="black", alpha=0.5) +
  geom_line(data = final_forecast, aes(x = Date, y = TES, color = "TES"), size = 1) +
  labs(
    title = "Hotels and Motels - Training and Testing Sets",
    x = "Date",
    y = "Producer Price Index by Industry"
  ) +
  scale_color_manual(values = c("Data" = "#D6B304","TES" = "#000ADE"), name = "Index") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))










