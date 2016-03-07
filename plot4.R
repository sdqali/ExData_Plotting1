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

# Set layout
par(mfrow = c(2, 2))


# First plot
gap <- subset(validData, !is.na(Global_active_power)) 

# Create dateTimeColumn
gap$dateTime <- strptime(paste(gap$Date, gap$Time), format = "%Y-%m-%d %H:%M:%S")
# Compute minutes elapsed
gap$minutesElapsed <- difftime(gap$dateTime, startDate, units = "mins")

# Build plot
plot(x = gap$minutesElapsed, 
     y = gap$Global_active_power, 
     type = "l", 
     ylab = "Global Active Power", 
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

#################################################


###
# Second plot
###
gap <- subset(validData, !is.na(Voltage)) 

# Create dateTimeColumn
gap$dateTime <- strptime(paste(gap$Date, gap$Time), format = "%Y-%m-%d %H:%M:%S")
# Compute minutes elapsed
gap$minutesElapsed <- difftime(gap$dateTime, startDate, units = "mins")

# Build plot
plot(x = gap$minutesElapsed, 
     y = gap$Voltage, 
     type = "l", 
     ylab = "Voltage", 
     xlab = "datetime",
     axes = FALSE,
     frame.plot = TRUE)

# Build x axis
xTicks <- seq(0, max(gap$minutesElapsed), length.out = 3)
xLabels <- c("Thu", "Fri", "Sat")
axis(side = 1, labels = xLabels, at = xTicks)

# Build y axis
yTicks = seq(234, 246, by = 4)
axis(side = 2, labels = yTicks, at = yTicks)

#################################################

# Third plot
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
       x = 700,
       y = 40,
       bty = "n")


#################################################


###
# Fourth plot
###
gap <- subset(validData, !is.na(Global_reactive_power)) 

# Create dateTimeColumn
gap$dateTime <- strptime(paste(gap$Date, gap$Time), format = "%Y-%m-%d %H:%M:%S")
# Compute minutes elapsed
gap$minutesElapsed <- difftime(gap$dateTime, startDate, units = "mins")

# Build plot
plot(x = gap$minutesElapsed, 
     y = gap$Global_reactive_power, 
     type = "l", 
     ylab = "Global_reactive_power", 
     xlab = "datetime",
     axes = FALSE,
     frame.plot = TRUE)

# Build x axis
xTicks <- seq(0, max(gap$minutesElapsed), length.out = 3)
xLabels <- c("Thu", "Fri", "Sat")
axis(side = 1, labels = xLabels, at = xTicks)

# Build y axis
yTicks = seq(0.0, 0.5, by = 0.1)
axis(side = 2, labels = yTicks, at = yTicks)



# Copyt to PNG
dev.copy(png, "plot4.png")
dev.off()
