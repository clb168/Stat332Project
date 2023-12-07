
library(plotly)
library(ggplot2)
library(TTR)
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


#ARIMA plot


#montly averages
my_data <- read.csv("/Users/cburh/Documents/Assignments_Fall2023/Stat_332/Project/SBUX.csv")
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

training_set <- my_dataframe[1:100, ]  # First 100 rows for training
testing_set <- my_dataframe[101:120, ]

print(training_set)
print(testing_set)





