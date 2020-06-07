plotter <- function(){
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip" 
  zipname <- "./data/Dataset.zip"
  if(!any(installed.packages()=="dplyr")){
    install.packages("dplyr", dependencies = T)
  }
  if(!any(installed.packages()=="lubridate")){
    install.packages("lubridate", dependencies = T)
  }


  
  if(!file.exists("./data")){
    
    dir.create("./data")
    download.file(url,zipname)
    
    # The script automatically looks for and validates if the selected directory for the file is in the computer. If it is not, it's added, along with the file
    
  }
  library(dplyr)
  library(datasets)
  library(lubridate)
  
  
  if(!file.exists("Dataset_Power_Consumption.zip")){
    unzip(zipfile =zipname,exdir = "./data")
    
  }
  path <- file.path("./data","household_power_consumption.txt")
  datpowcons <<- read.table(path,sep = ";",header = T)
  datpowcons$Date <- as.Date(datpowcons$Date, format = "%d/%m/%Y")
  datpowcons$Time<- strptime(datpowcons$Time, format = "%H:%M:%S")
  
  datpowcons <- datpowcons %>% 
    mutate(Format_Date = as.Date(datpowcons$Date, format = "%d/%m/%Y"))
  W_dat <- as.character(datpowcons$Format_Date)
  wSub <- (grepl("2007-02-01",W_dat)|grepl("2007-02-02",W_dat))

  mergedpc <- datpowcons[wSub,]
  
  mergedpc[1:1440,"Time"] <- format(mergedpc[1:1440,"Time"],"2007-02-01 %H:%M:%S")
  mergedpc[1441:2880,"Time"] <- format(mergedpc[1:1440,"Time"],"2007-02-02 %H:%M:%S")
  
  
  #THe codes to plot are presented here
  
  par(mfrow = c(1,1))
  with(mergedpc, hist(as.numeric(Global_active_power),main = "Global Active Power", xlab = "Global Active Power (Kilowatts)", ylab = "Frequence", col = "red"))
  
  dev.copy(png, file = "Plot1.png")
  dev.off()
  # Plot 1
  
  par(mfrow = c(1,1))
  plot(mergedpc$Time,as.character(as.numeric(mergedpc$Global_active_power)),type = "l",xlab = "",ylab = "Global Active Power (kilowatts)", main = "Global Active Power in Time")
  dev.copy(png, file = "Plot2.png")
  dev.off()
  #Plot 2
  par(mfrow = c(1,1))
  with(mergedpc, {
    plot(Time,Sub_metering_1,type = "n",xlab = "",ylab="Energy Sub Metering")
    lines(Time,as.numeric(as.character(Sub_metering_1)))
    lines(Time,as.numeric(as.character(Sub_metering_2)),col = "red")
    lines(Time,as.numeric(as.character(Sub_metering_3)),col = "blue")
    legend("topright",lty = 1, col = c("Sub Metering 1", "Sub Metering 2", "Sub Metering 3"))
    })
  title(main = "Energy Sub-Metering")
  dev.copy(png, file = "Plot3.png")
  dev.off()
  
  
  #Plot 3
  
  par(mfrow = c(2,2))
  with(mergedpc, {
    plot(Time,Global_active_power,type = "l",xlab = "",ylab="global active power")
    
    plot(Time,as.numeric(as.character(Voltage)),type = "l",xlab="datetime",ylab = "Voltage")
    plot(Time,as.numeric(as.character(Sub_metering_1)),type = "n",xlab="datetime",ylab = "Sub Metering")
    with(mergedpc,lines(Time,as.numeric(as.character(Sub_metering_1))))
    with(mergedpc,lines(Time,as.numeric(as.character(Sub_metering_2)),col="red"))
    with(mergedpc,lines(Time,as.numeric(as.character(Sub_metering_3)),col="blue"))
    legend("topright", lty=1, col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), cex = 0.6)
    plot(mergedpc$Time, as.numeric(as.character(mergedpc$Global_reactive_power)),type = "l", xlab = "datetime", ylab = "Global reactive power")
  })
  
  dev.copy(png, file = "Plot4.png")
  dev.off()
  #Plot 4
  
  
}