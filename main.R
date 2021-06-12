## Author: Dwaraknaath Varadharajan
## dwarakvaradharajan@gmail.com




# Sets the file location as working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load Libraries
library(readr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(lubridate)
library(xgboost)
library(prophet)
library(Metrics)



# Environment Variables
relativeInputPath <- "Data/Input/"
relativeOutputPath <- "Data/Output/"

################ Helper Methods #########################

readInputCsvFiles <- function() {
  fileList <- list.files(relativeInputPath, pattern = "*.csv")
  df <-
    paste0(relativeInputPath, fileList) %>%
    map_df(~ fread(.))

  return(df)
}

exportOutputCsvFiles <- function(df, fileName) {
  write_csv(df, paste0(relativeOutputPath, fileName))
}


################# Task 1 ################################

df <- readInputCsvFiles()

################### Task 2 ##############################

averagePrice <-
  df %>%
  mutate(Year = year(df$Date), Month = month(df$Date)) %>%
  group_by(SettlementPoint, Year, Month) %>%
  summarise(AveragePrice = mean(Price))

################# Task 3 ###############################

exportOutputCsvFiles(averagePrice, "AveragePriceByMonth.csv")

################# Task 4 ###############################

hourlyVolatility <-
  df %>%
  mutate(Year = year(df$Date)) %>%
  dplyr::filter(Price > 0 & !grepl("LZ_", SettlementPoint)) %>%
  group_by(SettlementPoint, Year) %>%
  summarise(HourlyVolatility = sd(log(Price)))



################# Task 5 ###############################

exportOutputCsvFiles(hourlyVolatility, "HourlyVolatilityByYear.csv")

################# Task 6 ###############################

maxVolatility <-
  hourlyVolatility %>%
  group_by(Year) %>%
  summarise(HourlyVolatility = max((HourlyVolatility))) %>%
  left_join(hourlyVolatility) %>%
  exportOutputCsvFiles("MaxVolatilityByYear.csv")


################# Task 7 ###############################

for (Location in unique(df$SettlementPoint)) {
  spotFiles <- df %>%
    filter(SettlementPoint == Location) %>%
    mutate(Hour = paste0("X", lubridate::hour((Date)) + 1)) %>%
    mutate(Date = as.Date(Date)) %>%
    rename(Variable = SettlementPoint) %>%
    pivot_wider(names_from = Hour, values_from = Price) %>%
    relocate(Variable, .before = Date) %>%
    exportOutputCsvFiles(paste0("formattedSpotHistory/spot_", Location, ".csv"))
}


############## Bonus Mean Plots ####################

averagePrice %>%
  filter(grepl("HB_", SettlementPoint)) %>%
  mutate(Date = ymd(paste0(Year, "-", Month, "-", "01"))) %>%
  ggplot(aes(x = Date, y = AveragePrice, color = SettlementPoint)) +
  geom_line() +
  ylab("Average Price ($/MWh)")


ggsave(paste0(relativeOutputPath, "SettlementHubAveragePriceByMonth.png"))

averagePrice %>%
  filter(grepl("LZ_", SettlementPoint)) %>%
  mutate(Date = ymd(paste0(Year, "-", Month, "-", "01"))) %>%
  ggplot(aes(x = Date, y = AveragePrice, color = SettlementPoint)) +
  geom_line() +
  ylab("Average Price ($/MWh)")

ggsave(paste0(relativeOutputPath, "LoadZoneAveragePriceByMonth.png"))

############## Bonus Volatility Plots ####################
hourlyVolatility %>%
  filter(grepl("HB_", SettlementPoint)) %>%
  ggplot(aes(x = Year, y = HourlyVolatility, fill = SettlementPoint)) +
  geom_col(position = "dodge") +
  ggsave(paste0(relativeOutputPath, "ColmunChartHourlyVolatility.png"))

############# Bonus - Hourly Shape Profile Computation ################


for (Location in unique(df$SettlementPoint)) {
  hourlyShapeProfile1 <- df %>%
    filter(SettlementPoint == Location) %>%
    mutate(month_of_year = month(Date), day_of_week = weekdays(as.Date(Date)), Hour = hour(Date)) %>%
    group_by(SettlementPoint, month_of_year, day_of_week) %>%
    summarise(NormalizedPrice = mean(Price))

  hourlyShapeProfile2 <- df %>%
    filter(SettlementPoint == Location) %>%
    mutate(month_of_year = month(Date), day_of_week = weekdays(as.Date(Date)), Hour = hour(Date)) %>%
    group_by(SettlementPoint, month_of_year, day_of_week, Hour) %>%
    summarise(AveragePrice = mean(Price))

  hourlyShapeProfile <- left_join(hourlyShapeProfile2, hourlyShapeProfile1) %>%
    mutate(NormalizedPrice = AveragePrice / NormalizedPrice) %>%
    select(-AveragePrice) %>%
    exportOutputCsvFiles(paste0("hourlyShapreProfiles/profile_", Location, ".csv"))
}

################## Machine Learning using Prophet ###################
settlementLocation <- "HB_WEST"
forecastWindow <- 48

# Last 2 year of price is used as training data

df_train <- df %>%
  filter(SettlementPoint == settlementLocation & Date >='2018-01-01') %>%
  rename(y = Price, ds = Date) %>%
  head(-forecastWindow)

######## Model Training ##############
m <- prophet(df_train)

future <- make_future_dataframe(m, periods = 48, freq = "h")
# tail(future)
################# Forecasting prices for the next 48 hours #######################

forecast <- predict(m,future)

################### Results plotted and exported ################
plot(m, forecast)

performance <- df %>% 
  filter(SettlementPoint == settlementLocation & Date >='2018-01-01') %>%
  rename(y = Price) %>%
  mutate(ds = ymd_hms(Date)) %>%
  inner_join(forecast) %>%
  head(forecastWindow)


print(rmse(performance$y,performance$yhat))

exportOutputCsvFiles(forecast, paste0("ForecastResults/",settlementLocation,"_PriceForecast.csv"))


#################### THE END ##########################