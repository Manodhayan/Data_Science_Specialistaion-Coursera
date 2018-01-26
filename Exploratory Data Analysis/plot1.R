#Load power consumption data
powcon <- read.table("household_power_consumption.txt",skip=1,sep=";")
names(powercon) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
twodays <- subset(powercon,powercon$Date=="1/2/2007" | powercon$Date =="2/2/2007")

#Basic plot function
hist(as.numeric(as.character(twodays$Global_active_power)),col="red",main="Global Active Power",xlab="Global Active Power(kilowatts)")

# Graph Annotation
title(main="Global Active Power")

