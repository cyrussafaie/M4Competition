# Note ###########################################################
# 
# The R package: seer is built to run the all codes. The seer 
# package is available at: https://github.com/thiyangt/seer
#
#
# Note: download the data folder from: https://github.com/thiyangt/M4Competition/tree/master/data
# Preparation of these training files ae computationally expensive hence file are uploaded 
# to the above mentioned git repository. Further, codes to construct this training data files are
# available at: src floder of the gitrepository thiyangt/M4Competition
#
# Before you start download the data folder from:
# https://github.com/thiyangt/M4Competition/tree/master/data
##################################################################

#---- load and install necessary packages
library(tidyverse)
library(forecast)
library(Mcomp)
library(forecTheta)
library(foreach)
library(readr)
devtools::install_github("robjhyndman/tsfeatures")
library(tsfeatures)
devtools::install_github("thiyangt/seer")
library(seer)

#---- load data
## load M4 competition data
data(M4)
yearly_m4 <- subset(M4, "yearly")
quarterly_m4 <- subset(M4, "quaterly")
monthly_m4 <- subset(M4, "monthly")
weekly_m4 <- subset(M4, "weekly")
daily_m4 <- subset(M4, "daily")
hourly_m4 <- subset(M4, "hourly")

# load training data files
y_train <- load(file="data/yearly_training.rda") 
q_train <- load(file="data/quarterly_training.rda")
m_train <- load(file="data/monthly_training.rda")
w_train <- load(file="data/weekly_training.rda")
d_train <- load(file="data/daily_training.rda")
h_train <- load(file="data/hourly_training.rda")

#---- generate foecasts for yearly series
features_M4Y<- seer::cal_features(yearly_m4, database="M4", highfreq = FALSE)
yearly_rcp <- seer::build_rf(yearly_training, features_M4Y, rf_type="rcp", ntree=1000, seed=1)
predictions_yearlym4_rcp <- yearly_rcp$predictions
m4yearly_forecast <- seer::rf_forecast(predictions_yearlym4_rcp,
                                 yearly_m4, "M4", "cal_MASE", h=6,
                                 accuracy = FALSE)
yfmean <- m4yearly_forecast$mean
yfmean[yfmean < 0] <- 0
ylower <- m4yearly_forecast$lower
ylower[ylower < 0] <- 0
yupper <- m4yearly_forecast$upper
yupper[yupper < 0] <- 0

#---- generate foecasts for quarterly series
features_M4Q<- seer::cal_features(quarterly_m4, seasonal=TRUE, m=4,lagmax=5L, 
                            database="M4", highfreq = FALSE)
quarterly_rcp <- seer::build_rf(quarterly_training, features_M4Q, rf_type="rcp", ntree=1000, seed=1)
predictions_quarterlym4_rcp <- quarterly_rcp$predictions
m4quarterly_forecast <- rf_forecast(predictions_quarterlym4_rcp,
                                    quarterly_m4, "M4", "cal_MASE", h=8,
                                    accuracy = FALSE)
qfmean <- m4quarterly_forecast$mean
qfmean[qfmean < 0] <- 0
qlower <- m4quarterly_forecast$lower
qlower[qlower < 0] <- 0
qupper <- m4quarterly_forecast$upper
qupper[qupper < 0] <- 0

#---- generate foecasts for monthly series
features_M4M<- seer::cal_features(monthly_m4, seasonal=TRUE, m=12,lagmax=13L, 
                            database="M4", highfreq = FALSE)
monthly_rcp <- seer::build_rf(monthly_training, features_M4M, rf_type="rcp", ntree=1000, seed=1)
predictions_monthlym4_rcp <- monthly_rcp$predictions
m4monthly_forecast <- seer::rf_forecast(predictions_monthlym4_rcp, monthly_m4, "M4", "cal_MASE", h=18,
                     accuracy = FALSE)
mfmean <- m4monthly_forecast$mean
mfmean[mfmean < 0] <- 0
mlower <- m4monthly_forecast$lower
mlower[mlower < 0] <- 0
mupper <- m4monthly_forecast$upper
mupper[mupper < 0] <- 0

#---- generate foecasts for weekly series
features_M4W <- seer::cal_features(weekly_m4, seasonal=TRUE, m=52,lagmax=53L, 
                             database="M4", highfreq = FALSE)
weekly_rcp <- seer::build_rf(weekly_training, features_M4W, rf_type="rcp", ntree=1000, seed=1)
predictions_weeklym4_rcp <- weekly_rcp$predictions
m4weekly_forecast <- seer::rf_forecast(predictions_weeklym4_rcp,
                                 weekly_m4, "M4", "cal_MASE", h=13,
                                 accuracy = FALSE)
wfmean <- m4weekly_forecast$mean
wfmean[wfmean < 0] <- 0
wlower <- m4weekly_forecast$lower
wlower[wlower < 0] <- 0
wupper <- m4weekly_forecast$upper
wupper[wupper < 0] <- 0

#---- generate foecasts for daily series
## convert data into msts object
dailym4_msts <- lapply(daily_m4, function(temp){
  temp$x <- convert_msts(temp$x, "daily")
  return(temp)
})
features_M4D <- seer::cal_features(dailym4_msts, seasonal=TRUE, m=7,lagmax=8L, 
                             database="M4", highfreq = TRUE)
daily_rcp <- seer::build_rf(daily_training, features_M4D, rf_type="rcp", ntree=1000, seed=1)
predictions_dailym4_rcp <- daily_rcp$predictions
m4daily_forecast <- seer::rf_forecast(predictions_dailym4_rcp, dailym4_msts, "M4", "cal_MASE", h=14,
                     accuracy = FALSE)
dfmean <- m4daily_forecast$mean
dfmean[dfmean < 0] <- 0
dlower <- m4daily_forecast$lower
dlower[dlower < 0] <- 0
dupper <- m4daily_forecast$upper
dupper[dupper < 0] <- 0


#---- generate foecasts for hourly series
## convert data into msts object
hourlym4_msts <- lapply(hourly_m4, function(temp){
  temp$x <- convert_msts(temp$x, "hourly")
  return(temp)
})
features_M4H <- seer::cal_features(hourlym4_msts, seasonal=TRUE, m=24,lagmax=25L, 
                             database="M4", highfreq = TRUE)
hourly_rcp <- seer::build_rf(hourly_training, features_M4H, rf_type="rcp", ntree=1000, seed=1)
predictions_hourlym4_rcp <- hourly_rcp$predictions
m4hourly_forecast <- rf_forecast(predictions_hourlym4_rcp,
                                 hourlym4_msts, "M4", "cal_MASE", h=48,
                                 accuracy = FALSE)

#---- processing final csv files

