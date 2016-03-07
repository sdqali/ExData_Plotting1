# Construct a histogram of Frequency against Active Power for the
# Individual household electric power consumption Data Set

library(dplyr)

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

# Create dateTimeColumn
gap$dateTime <- strptime(paste(gap$Date, gap$Time), format = "%Y-%m-%d %H:%M:%S")
# Compute minutes elapsed
gap$minutesElapsed <- difftime(gap$dateTime, startDate, units = "mins")

# Build plot
plot(x = gap$minutesElapsed, 
     y = gap$Global_active_power, 
     type = "l", 
     ylab = "Global Active Power (kilowatts)", 
     xlab = "",
     axes = FALSE,
     frame.plot = TRUE)

# Build x axis
xTicks <- seq(0, max(gap$minutesElapsed), length.out = 3)
xLabels <- c("Thu", "Fri", "Sat")
axis(side = 1, labels = xLabels, at = xTicks)

# Build y axis
yTicks = c(0, 2, 4, 6)
axis(side = 2, labels = yTicks, at = yTicks)

# Copyt to PNG
dev.copy(png, "plot2.png")
dev.off()
