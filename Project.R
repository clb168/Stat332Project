
library(plotly)
library(ggplot2)

# Read CSV file
my_data <- read.csv("/Users/cburh/Documents/Assignments_Fall2023/Stat_332/Project/SBUX_1yr.csv")

# Convert the date column to Date type
my_data[,1] <- as.Date(my_data[,1], format="%m/%d/%Y")

# Create a data frame with the desired columns
my_dataframe <- data.frame(Time = my_data[,1], Value = my_data[,4])

# Ensure that Value is numeric (if it includes $ signs or commas, this step is necessary)
my_dataframe$Value <- as.numeric(gsub("[\\$,]", "", my_dataframe$Value))

# Plotting using ggplot
ggplot(my_dataframe, aes(x = Time, y = Value)) +
  geom_line() +  # This adds a line plot
  xlab("\n Date") +
  ylab("Price in dollars \n")

