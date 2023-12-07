
library(plotly)
library(ggplot2)
library(TTR)
library(lubridate)
library(tseries)
library(forecast)
# Read CSV file
#my_data <- read.csv("/Users/cburh/Documents/Assignments_Fall2023/Stat_332/Project/SBUX.csv")

#my_data[,1] <- as.Date(my_data[,1], format="%m/%d/%Y")

#my_dataframe <- data.frame(Time = my_data[,1], Value = my_data[,4])

#my_dataframe$Value <- as.numeric(gsub("[\\$,]", "", my_dataframe$Value))

# time series plot 
#ggplot(my_dataframe, aes(x = Time, y = Value)) +
  #geom_line() +  # This adds a line plot
  #xlab("\n Date") +
  #ylab("Price in dollars \n")


# decomposition
#decomp_data = SMA(my_dataframe, n=30)
#plot.ts(decomp_data)
#print(decomp_data)

#TESTING SEASONALIty
#d = fft(my_dataframe)
#decomposed_data=decompose(d,type='multiplicative')
#autoplot(fft(decomposed_data))

#partial auto correlation
#acf(my_dataframe, type = c("partial"), plot = TRUE)

#mean squared error

#montly averages
my_data <- read.csv("Stat332Project/SBUX.csv")
Open_ts = ts(my_data[,1], frequency = 1)

monthly_avg = array(0, dim = c(120))
month=month(mdy(my_data[,1]))
# Extract the 4th column
column_data <- my_data[, 4]

month_year = array(" ", dim = c(120))
year=year(mdy(my_data[,1]))
#month_year[120] = "12/2013"
# Remove the dollar sign and convert to numeric
numeric_data <- as.numeric(sub("\\$", "", column_data))
#print(month)
temp = 5
j=1

month_iter = 1
for (i in 1:length(month)){
  #print(i)
  if (i==1){
    monthly_avg[j] = numeric_data[1]
    #print(monthly_avg[j])
    
  }
  else if (month[i] == month[i-1]){
    monthly_avg[j] = monthly_avg[j] + numeric_data[i]
    #print(monthly_avg[j])
    month_iter = month_iter + 1
    if (i==length(month)){
      monthly_avg[j] = monthly_avg[j]/month_iter
      month_year[j] <- paste0("", month[i], "/", year[i], "")
    }
  }
  else if (month[i] != month[i-1]) {
    monthly_avg[j] = monthly_avg[j]/month_iter
    month_year[j] <- paste0("", month[i-1], "/", year[i-1], "")
    month_iter = 1
    j = j+1
    print(j)
    monthly_avg[j] = numeric_data[i]
  }
  
}



print(monthly_avg)
print(month_year)

month_yearRev = rev(month_year)
month_avgRev = rev(monthly_avg)
my_dataframe <- data.frame(Time = month_yearRev, Value = month_avgRev)
print(my_dataframe)
#time series plus decomposition plots
tsPlot = ts(my_dataframe[,2], frequency = 12)
plot(tsPlot)
decomp=decompose(tsPlot, type=c("multiplicative"))
plot(decomp)

#training_set <- my_dataframe[1:100, ]  # First 100 rows for training
#testing_set <- my_dataframe[101:120, ]

#print(training_set)
#print(testing_set)

#ACF and partial ACF
acf(my_dataframe, type = c("partial"), plot = TRUE)
acf(my_dataframe, type = c("correlation"), plot = TRUE)

#BoxTest
Box.test(tsPlot,lag=5,type = "Ljung-Box")
Box.test(tsPlot,lag=5,type = "Box-Pierce")

#fft stuff and splitting dataset for periodigram
SBX_seasonal = as.vector(decomp$seasonal)
#print(SBX_seasonal)

train = SBX_seasonal[1:100]
test = SBX_seasonal[101:120]
print(test)
print(train)

Training = fft(train)
require(LSTS)


TrainPgram = LSTS::periodogram(Training)
TrainPgram$plot

# which ARIMA plot should we use?
result <- auto.arima(monthly_avg,seasonal= TRUE)
print(result)
#ARIMA (0,1,1 plot)
arima1<- arima(monthly_avg, order=c(0,1,1))
checkresiduals(arima1)
# lets do another but with a seasonal model
arima2<-arima(monthly_avg, order=c(0,1,1), seasonal=c(0,1,1))
checkresiduals(arima2)
plot(forecast(arima2,h=12))


######### Gage stuff
#Stationarity Test
adf.test(my_data$Open)


#Trend Dcmp
data_seasonal = as.vector(decomp$seasonal)
data_trend = as.vector(decomp$trend)


#Training and Testing Sets
ntraining_set <- my_dataframe[21:120, ] 
ytraining_set = data_trend[]
ntesting_set <- my_dataframe[1:20, ]



#ACF and PACF of Time Series
acf(my_data_ts)
pacf(my_data_ts)

#Mean Square Error
acf((my_data_ts-mean(my_data_ts))^2)
pacf((my_data_ts-mean(my_data_ts))^2)
acf(my_data_ts, lag.max=40)
pacf(my_data_ts)





