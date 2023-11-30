
library(plotly)
library(ggplot2)
library(TTR)
# Read CSV file
my_data <- read.csv("/Users/cburh/Documents/Assignments_Fall2023/Stat_332/Project/SBUX_1yr.csv")

my_data[,1] <- as.Date(my_data[,1], format="%m/%d/%Y")

my_dataframe <- data.frame(Time = my_data[,1], Value = my_data[,4])

my_dataframe$Value <- as.numeric(gsub("[\\$,]", "", my_dataframe$Value))

# time series plot 
ggplot(my_dataframe, aes(x = Time, y = Value)) +
  geom_line() +  # This adds a line plot
  xlab("\n Date") +
  ylab("Price in dollars \n")


# decomposition
decomp_data = SMA(my_dataframe, n=10)
plot.ts(decomp_data)
print(decomp_data)

#partial auto correlation
acf(my_dataframe, type = c("partial"), plot = TRUE)


