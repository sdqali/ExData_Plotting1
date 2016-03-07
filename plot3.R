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
energyMetering <- subset(validData, !is.na(Sub_metering_1) 
                         & !is.na(Sub_metering_2) 
                         & !is.na(Sub_metering_3)) 

# Create dateTimeColumn
energyMetering$dateTime <- strptime(paste(energyMetering$Date, energyMetering$Time), format = "%Y-%m-%d %H:%M:%S")
# Compute minutes elapsed
energyMetering$minutesElapsed <- difftime(energyMetering$dateTime, startDate, units = "mins")

# Plot Sub_metering_1
plot(x = energyMetering$minutesElapsed, 
     y = energyMetering$Sub_metering_1, 
     type = "l", 
     ylab = "Energy sub metering", 
     xlab = "",
     axes = FALSE,
     frame.plot = TRUE)

# Plot Sub_metering_2
lines(energyMetering$Sub_metering_2, col = "red")

# Plot Sub_metering_3
lines(energyMetering$Sub_metering_3, col = "blue")

# Build x axis
xTicks <- seq(0, max(gap$minutesElapsed), length.out = 3)
xLabels <- c("Thu", "Fri", "Sat")
axis(side = 1, labels = xLabels, at = xTicks)

# Build y axis
yTicks = c(0, 10, 20, 30)
axis(side = 2, labels = yTicks, at = yTicks)

# Add legend
legend(legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       lty = c(1, 1, 1),
       col = c("black", "red", "blue"),
       x = 1652, 
       y = 40,
       y.intersp = 1.5)

# Copyt to PNG
dev.copy(png, "plot3.png")
dev.off()
