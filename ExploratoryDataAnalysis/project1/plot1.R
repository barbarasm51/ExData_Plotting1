setwd("~/Desktop/Coursera/ExploratoryDataAnalysis/project1")

## set up file to be read according to environment in use

home="~/Desktop/Coursera/ExploratoryDataAnalysis/household_power_consumption.txt"
work="~/household_power_consumption.txt"
file=home

## determine which rows should be included in analysis file, i.e. those for 2/1/2007 and 2/2/2007

data <- read.table(file, sep=";", quote="\"",colClasses=c("character",rep("NULL",8)),header=TRUE)
start="1/2/2007"
end="2/2/2007"
data1=subset(data,data$Date==start)
data2=subset(data,data$Date==end)
first=rownames(data1)
first=as.numeric(first[1])
last=rownames(data2)
last=as.numeric(last[length(last)])

## set up column classes for analysis file
## set up column names for analysis file since we'll be skipping the header row

column=c(rep("character",2),rep("numeric",7))
namecol=c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")

## read in the rows for 2/1/2007 and 2/2/2007

alldata <- read.table(file, sep=";", quote="\"",colClasses=column,col.names=namecol,header=FALSE,skip=first,nrows=last-first+1)

## fix date and time to be POSIXlt classes

alldata=transform(alldata,newdate=as.POSIXlt(strptime(Date,"%d/%m/%Y")))
alldata=transform(alldata,newtime=as.POSIXlt(strptime(Time,"%H:%M:%S")))
alldata=transform(alldata,datetime=as.POSIXlt(paste(alldata$Date,alldata$Time),format="%d/%m/%Y %H:%M:%S"))

## create plot1 png file

png(filename="~/Desktop/Coursera/ExploratoryDataAnalysis/project1/plot1.png",height=480,width=480,bg="white")
plot1=hist(alldata$Global_active_power,col="red",xlab="Global Active Power (kilowatts)",main="Global Active Power")
dev.off()


