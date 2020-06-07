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



par(mfrow = c(1,1))
plot(mergedpc$Time,as.character(as.numeric(mergedpc$Global_active_power)),type = "l",xlab = "",ylab = "Global Active Power (kilowatts)", main = "Global Active Power in Time")
dev.copy(png, file = "Plot2.png", width = 480, height =480)
dev.off()
#Plot 2