# plot1.R

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
           Global_active_power=as.numeric(Global_active_power)) %>%
      filter(Date_Time >= as.POSIXct("01/02/2007 00:00:00",format='%d/%m/%Y %H:%M:%S'),
             Date_Time <  as.POSIXct("03/02/2007 00:00:00",format='%d/%m/%Y %H:%M:%S')) %>%
        select(Date_Time,Global_active_power) 
  
  x
}

# Function plotHist(dataframe)
#
# Purpose: Assumes that the graphics device is already set up
#
# Parameters: x the data set to plot
plotHist<-function(x) {
  
  hist(x$Global_active_power,col="red",ylab="Frequency",
       xlab="Global Active Power (kilowatts)",
       main="Global Active Power")
  
}

main<-function() {
  downloadData("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","power.zip",FALSE)
  
  x<-readData("power.zip","household_power_consumption.txt")
  # Default is 480x480 which is as per spec no need to specify 
  png("plot1.png")
  plotHist(x)
  dev.off()
}

x<-main()

  