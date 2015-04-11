# plot4.R

library(dplyr)
# Function downloadData(url,toFile,overwrite=False)
#
# Purpose: downloads and saves the file to the current working directory unzipping it
#          if it doesn't exist or overwrite = True
#
# Parameters: url - The url of the file
#             toFile - the filename of the file to save 
#             overwrite - if TRUE it will overwrite
#                         if FALSE and file already exists will not download
#
downloadData<-function(url, toFile,overwrite=False) {
  if ( ! file.exists(toFile) || overwrite == TRUE) {
    if ( file.exists(toFile) ) {
      file.remove(toFile)
    }
    download.file(url,toFile)
    
  }
}

# Function readData(zipFile,dataFile)
#
# Purpose: Reads the data set from the zip file and restricts the data to
#
# Parameters: zipFile - the zip file downloaded that contains the data set
#             dataFile - the data file within the zip file that needs to be read
#
#
# Returns:   x - data frame consisting of only the data for dates 1-Feb-2007 & 2-Feb-2007
#
readData<-function(zipFile,dataFile) {
  
  con<-unz(zipFile,dataFile)
  x<-read.table(con,header=TRUE,sep=';',
                colClasses=c('character','character','character','character','character',
                             'character','character','character','character'))
  
  # Convert the Date and Time columns to a single column Date_Time
  
  x<-filter(x,Date != '?',Time != '?',Global_active_power !='?') %>%
    mutate(Date_Time=as.POSIXct(paste(Date,Time,sep=" "),format='%d/%m/%Y %H:%M:%S'),
           Global_active_power=as.numeric(Global_active_power),
           Global_reactive_power=as.numeric(Global_reactive_power),
           Voltage=as.numeric(Voltage),
           Sub_metering_1=as.numeric(Sub_metering_1),
           sub_metering_2=as.numeric(Sub_metering_2),
           sub_metering_3=as.numeric(Sub_metering_3)) %>%
    filter(Date_Time >= as.POSIXct("01/02/2007 00:00:00",format='%d/%m/%Y %H:%M:%S'),
           Date_Time <  as.POSIXct("03/02/2007 00:00:00",format='%d/%m/%Y %H:%M:%S')) %>%
    select(Date_Time,Global_active_power,Sub_metering_1,Sub_metering_2,Sub_metering_3,Voltage,
           Global_reactive_power) 
  
  x
}

# Function plotGlobalActivePower(dataframe)
#
# Purpose: Assumes that the graphics device is already set up
#
# Parameters: x the data set to plot
plotGlobalActivePower<-function(x) {
  
  plot(x$Date_Time,x$Global_active_power,col="black",
       xlab="",
       ylab="Global Active Power",
       type="l",xaxt="n")
  d<-seq(as.POSIXct("2007-02-01"),as.POSIXct("2007-02-03"),by="day")
  axis.POSIXct(1,at=d,format="%a")
  
}
# Function plotVoltage(dataframe)
#
# Purpose: Assumes that the graphics device is already set up
#
# Parameters: x the data set to plot
plotVoltage<-function(x) {
  
  plot(x$Date_Time,x$Voltage,col="black",
       xlab="datetime",
       ylab="Voltage",
       type="l",xaxt="n")
  d<-seq(as.POSIXct("2007-02-01"),as.POSIXct("2007-02-03"),by="day")
  axis.POSIXct(1,at=d,format="%a")
  
}


# Function plotSubMetering(zipFile,dataFile)
#
# Purpose: Assumes that the graphics device is already set up
#
# Parameters: x the data set to plot
plotSubMetering<-function(x) {
  
  plot(x$Date_Time,x$Sub_metering_1,col="black",
       xlab="",
       ylab="Energy sub metering",
       type="l",xaxt="n")
  lines(x$Date_Time,x$Sub_metering_2,col="red")
  lines(x$Date_Time,x$Sub_metering_3,col="blue")
  d<-seq(as.POSIXct("2007-02-01"),as.POSIXct("2007-02-03"),by="day")
  axis.POSIXct(1,at=d,format="%a")
  legend("topright",c("sub_metering_1","sub_metering_2","sub_metering_3"),
         lty=c(1,1),col=c("black","red","blue"),bty="n")
  
}

# Function plotGlobalReactivePower(dataframe)
#
# Purpose: Assumes that the graphics device is already set up
#
# Parameters: x the data set to plot
plotGlobalReactivePower<-function(x) {
  
  plot(x$Date_Time,x$Global_reactive_power,col="black",
       xlab="datetime",
       ylab="Global_reactive_power",
       type="l",xaxt="n")
  d<-seq(as.POSIXct("2007-02-01"),as.POSIXct("2007-02-03"),by="day")
  axis.POSIXct(1,at=d,format="%a")
  
}

main<-function() {
  downloadData("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","power.zip",FALSE)
  
  x<-readData("power.zip","household_power_consumption.txt")
  # Default is 480x480 which is as per spec no need to specify 
  png("plot4.png")
  
  # Plot 2 rows by 2 cols
  
  old.par<-par(mfrow=c(2,2))
  plotGlobalActivePower(x)
  plotVoltage(x)
  plotSubMetering(x)
  plotGlobalReactivePower(x)
  par(old.par)
  dev.off()
  x
}

x<-main()
