library(plotly)
library(ggplot2)
library(TTR)
library(lubridate)
library(tseries)
library(forecast)

#monthly averages
my_data <- read.csv("/Users/zbrown/classes/STAT332/Stat332Project/SBUX.csv")
#my_data <- read.csv("/Users/cburh/Documents/Assignments_Fall2023/Stat_332/Project/SBUX.csv")
Open_ts <- ts(my_data[, 1], frequency = 1)

monthly_avg <- array(0, dim = c(120))
month <- month(mdy(my_data[, 1]))
# Extract the 4th column
column_data <- my_data[, 4]

week <- week(mdy(my_data[, 1]))
month_year <- array(" ", dim = c(120))
year <- year(mdy(my_data[, 1]))
# Remove the dollar sign and convert to numeric
numeric_data <- as.numeric(sub("\\$", "", column_data))
j <- 1

month_iter <- 1
for (i in 1:length(month)) {
  if (i == 1) {
    monthly_avg[j] <- numeric_data[1]
  }
  else if (month[i] == month[i - 1]) {
    monthly_avg[j] <- monthly_avg[j] + numeric_data[i]
    month_iter <- month_iter + 1
    if (i == length(month)) {
      monthly_avg[j] <- monthly_avg[j] / month_iter
      month_year[j] <- paste0("", month[i], "/", year[i], "")
    }
  }
  else if (month[i] != month[i - 1]) {
    monthly_avg[j] <- monthly_avg[j] / month_iter
    month_year[j] <- paste0("", month[i - 1], "/", year[i - 1], "")
    month_iter <- 1
    j <- j + 1
    monthly_avg[j] <- numeric_data[i]
  }
}

month_yearRev <- rev(month_year)
month_avgRev <- rev(monthly_avg)
my_dataframe <- data.frame(Time = month_yearRev, Value = month_avgRev)
print(my_dataframe)
#time series plus decomposition plots
tsPlot <- ts(my_dataframe[, 2], frequency = 12)
plot(tsPlot)
decomp <- decompose(tsPlot, type = c("multiplicative"))
plot(decomp)

#ACF and PACF of Time Series
acf(tsPlot)
pacf(tsPlot)

#Mean Square Error
acf((tsPlot - mean(tsPlot))^2)
pacf((tsPlot - mean(tsPlot))^2)
acf(tsPlot, lag.max = 40)
pacf(tsPlot)

#BoxTest
Box.test(tsPlot, lag = 5, type = "Ljung-Box")
Box.test(tsPlot, lag = 5, type = "Box-Pierce")

#fft stuff and splitting dataset for periodigram
SBX_seasonal <- as.vector(decomp$seasonal)

train <- SBX_seasonal[1:90]
test <- SBX_seasonal[91:121]
print(test)
print(train)

Training <- fft(train)
require(LSTS)

TrainPgram <- LSTS::periodogram(Training)
TrainPgram$plot

# # use auto ARIMA to figure out which ARIMA to use.
# result <- auto.arima(month_avgRev,seasonal= TRUE)
# print(result)
# 
# #ARIMA (0,1,1 plot)
# arima1<- arima(month_avgRev, order=c(0,1,1))
# checkresiduals(arima1)
# # lets do another but with a seasonal model
# arima2<-arima(month_avgRev, order=c(0,1,1), seasonal=c(0,1,1))
# checkresiduals(arima2)
# plot(forecast(arima2,h=20))

# Stationary Test
adftest <- adf.test(month_avgRev)
print(adftest)

#taking difference of data to get stationary
adftest_diff <- adf.test(diff(month_avgRev))
print(adftest_diff)

## get best garch
get.best.garch = function(x.ts, maxord = c(1, 1)) {
  best.aic = 1e8
  n = length(x.ts)
  for (p in 0:maxord[1]) {
    for (q in 0:maxord[2]) {
      if (p + q != 0) {
        fit <- garch(x.ts, order = c(p, q), trace = F)
        fit.aic <- 2 * fit$n.likeli + (log(fit$n.used) + 1) * length(fit$coef)
        if (fit.aic < best.aic) {
          best.aic <- fit.aic
          best.fit <- fit
          best.model <- c(p, q)
          best.model.res <- resid(fit)[-(1:p)]
        }
      }
    }
  }
  list(best.aic, best.fit, best.model, best.model.res)
}

SBUX_best_garch <- get.best.garch(tsPlot, maxord = c(1, 2))
acf(SBUX_best_garch[[4]]^2)

#second order
# adftest_diff2 <- adf.test(diff(diff(monthly_avg)))
# print(adftest_diff2)
# 
#taking log of data
# adftest_log <- adf.test(log(month_avgRev))
# print(adftest_diff2)

#get best arima
get.best.arima <- function(x.ts, maxord = c(1, 1, 1, 1, 1, 1)) {
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for (d in 0:maxord[2]) for (q in 0:maxord[3])
    for (P in 0:maxord[4]) for (D in 0:maxord[5]) for (Q in 0:maxord[6]) {
      fit <- arima(x.ts, order = c(p, d, q),
                  seas = list(order = c(P, D, Q),
                              frequency(x.ts)), method = "CSS")
      fit.aic = -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (fit.aic < best.aic) {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p, d, q, P, D, Q)
      }
    }
  list(best.aic, best.fit, best.model)
}

#get best of entire dataset
get.best.arima(tsPlot)

#splitting into test and training

ntraining_set <- my_dataframe[1:100, ] 
ntesting_set <- my_dataframe[101:120, ]

#get best of training
ts_training <- ts(ntraining_set[, 2], frequency = 12)
get.best.arima(ts_training)

#plotting arima
#ARIMA (0,1,1 plot)
arima1<- arima(ts_training, order=c(0,1,1))
checkresiduals(arima1)
# lets do another but with a seasonal model
arima2<-arima(ts_training, order=c(0,1,1), seasonal=c(0,1,1))
checkresiduals(arima2)
#forecast plot - going to overlay the testing set
ts_testing <-ts(ntesting_set[,2], frequency = 12)
plot(forecast(arima2,h=20))
points(tsPlot)





