# Construct a histogram of Frequency against Active Power for the
# Individual household electric power consumption Data Set

# load data
rawData <- read.csv("household_power_consumption.txt", 
                    sep = ";", 
                    header = TRUE, 
                    stringsAsFactors = FALSE, 
                    na.strings = c("?"))

# Convert to dates
rawData$Date <- strptime(rawData$Date, format = "%d/%m/%Y")

#Extract data for the dates we are interested in
startDate <- strptime("2007-02-01", format = "%Y-%m-%d")
endDate <- strptime("2007-02-02", format = "%Y-%m-%d")
validData <- subset(rawData, Date >= startDate & Date <= endDate)


# Filter out NA values
gap <- subset(validData, !is.na(Global_active_power)) 

# Plot a histogram
hist(gap$Global_active_power, 
     col = "red", 
     freq = TRUE, 
     ylab = "Frequency",
     xlab = "Global Active Power (kilowatts)",
     main = "Global Active Power", 
     xlim = c(0, 8),
     axes = FALSE,
     breaks = 20)

# Draw x axis
xTicks <- c(0, 2, 4, 6)
axis(side = 1, labels = xTicks, at = xTicks)

# Draw y axis
yTicks <- seq(0, 1200, length.out = 7)
axis(side = 2, labels = yTicks, at = yTicks)

# Copyt to PNG
dev.copy(png, "plot1.png")
dev.off()


