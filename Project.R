library(plotly)
library(ggplot2)
library(TTR)
library(lubridate)
library(tseries)
library(forecast)

#monthly averages
my_data <- read.csv("SBUX.csv")
Open_ts <- ts(my_data[, 1], frequency = 1)

monthly_avg <- array(0, dim = c(120))
month <- month(mdy(my_data[, 1]))
# Extract the 4th column
column_data <- my_data[, 4]

week <- week(mdy(my_data[, 1]))
month_year <- array(" ", dim = c(120))
year <- year(mdy(my_data[,1]))
# Remove the dollar sign and convert to numeric
numeric_data <- as.numeric(sub("\\$", "", column_data))
j <- 1

month_iter <- 1
for (i in 1:length(month)){
  if (i==1){
    monthly_avg[j] <- numeric_data[1]
  }
  else if (month[i] == month[i-1]){
    monthly_avg[j] <- monthly_avg[j] + numeric_data[i]
    month_iter <- month_iter + 1
    if (i==length(month)){
      monthly_avg[j] <- monthly_avg[j]/month_iter
      month_year[j] <- paste0("", month[i], "/", year[i], "")
    }
  }
  else if (month[i] != month[i-1]) {
    monthly_avg[j] <- monthly_avg[j]/month_iter
    month_year[j] <- paste0("", month[i-1], "/", year[i-1], "")
    month_iter <- 1
    j <- j+1
    monthly_avg[j] <- numeric_data[i]
  }
}

month_yearRev <- rev(month_year)
month_avgRev <- rev(monthly_avg)
my_dataframe <- data.frame(Time = month_yearRev, Value = month_avgRev)
print(my_dataframe)
#time series plus decomposition plots
tsPlot <- ts(my_dataframe[,2], frequency = 12)
plot(tsPlot)
decomp <- decompose(tsPlot, type=c("multiplicative"))
plot(decomp)

#ACF and PACF of Time Series
acf(tsPlot)
pacf(tsPlot)

#Mean Square Error
acf((tsPlot-mean(tsPlot))^2)
pacf((tsPlot-mean(tsPlot))^2)
acf(tsPlot, lag.max=40)
pacf(tsPlot)

#BoxTest
Box.test(tsPlot,lag=5,type = "Ljung-Box")
Box.test(tsPlot,lag=5,type = "Box-Pierce")

#fft stuff and splitting dataset for periodigram
SBX_seasonal <- as.vector(decomp$seasonal)

train <- SBX_seasonal[1:370]
test <- SBX_seasonal[371:529]
print(test)
print(train)

Training <- fft(train)
require(LSTS)

TrainPgram <- LSTS::periodogram(Training)
TrainPgram$plot

# use auto ARIMA to figure out which ARIMA to use.
result <- auto.arima(month_avgRev,seasonal= TRUE)
print(result)
#ARIMA (0,1,1 plot)
arima1<- arima(month_avgRev, order=c(0,1,1))
checkresiduals(arima1)
# lets do another but with a seasonal model
arima2<-arima(month_avgRev, order=c(0,1,1), seasonal=c(0,1,1))
checkresiduals(arima2)
plot(forecast(arima2,h=20))

# Stationary Test
adf.test(my_data$Open)

#Trend Decomposition
data_seasonal = as.vector(decomp$seasonal)
data_trend = as.vector(decomp$trend)

#Training and Testing Sets
ntraining_set <- my_dataframe[21:120, ] 
ytraining_set <- data_trend[]
ntesting_set <- my_dataframe[1:20, ]