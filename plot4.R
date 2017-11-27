plot4 <- function() {
    library(tictoc)
    library(datasets)
    message( cat("-------Start Generating Plot 4-------", date()))
    tic("Generate Plot 4")
    
    ##Check if old source data exists and delete if yes
    if(!file.exists("./sourceDataCourse4Week1")) {dir.create("./sourceDataCourse4Week1")}
    
    ##download and unzip the source data
    path <- "./sourceDataCourse4Week1"
    fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata/data/household_power_consumption.zip"
    download.file(fileUrl, file.path(path, "household_power_consumption.zip"), method = "curl")
    unzip(zipfile = file.path(path, "household_power_consumption.zip"))
    
    ##read lines, subset for first 2 days in Feb 2007 and read to a char data.frame
    Lines <- readLines("household_power_consumption.txt")
    subLines <- grep("^[12]/2/2007", substr(Lines,1,8))
    First2DaysFebData <- read.table(text = Lines[subLines])
    
    ##split the char data.frame to 9 columns char data.frame
    First2DaysFebData.temp <- lapply(First2DaysFebData, as.character)
    First2DaysFebData.temp2 <- strsplit(First2DaysFebData.temp[[1]],";")
    
    ##create a new data frame, with right data types for the data above
    mat <- matrix(unlist(First2DaysFebData.temp2), ncol = 9, byrow = TRUE)
    df <- as.data.frame(mat, stringsAsFactors = FALSE)
    power_consumption <- data.frame(
        "Date" = as.Date(df$V1, format = "%d/%m/%Y"),
        "Time" = strptime(paste(df$V1,df$V2), format = "%d/%m/%Y %H:%M:%S"),
        "Global_active_power" = as.double(df$V3),
        "Global_reactive_power" = as.double(df$V4),
        "Voltage" = as.double(df$V5),
        "Global_intensity" = as.double(df$V6),
        "Sub_metering_1" = as.double(df$V7),
        "Sub_metering_2" = as.double(df$V8),
        "Sub_metering_3" = as.double(df$V9)  )
    
##run Plot 4 graphs and save all as a single PNG named
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
    with(power_consumption, {
    ##[1,1] Plot 2
        plot(Time, Global_active_power, type = "l", ylab = "Global Active Power (kilowatts)", xlab = "")
    ##[1,2]
        plot(Time, Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
    ##[2,1] Plot 3   
        plot(Time, Sub_metering_1, type = "l", ylab = "Energy sub meeting", xlab = "")
        lines(Time,Sub_metering_2, col = "red")
        lines(Time,Sub_metering_3, col = "blue")
        ##legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black", "red", "blue"), lwd = 3, bty = "n", y.intersp = 0.3, box.lwd = 0)
        legend("topright", bty = "n" , lty = 1, lwd = 3, col=c("black", "red", "blue"), y.intersp = 0.3, legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex = 0.6)    
    ##[2,2]
        plot(Time, Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")    
    })
    invisible(dev.copy(png,"plot4.png"))
    invisible(dev.off())    
    
    toc()
    message( cat("-------Plot 4 is ready-------", date()))
    par(mfrow = c(1, 1))
}