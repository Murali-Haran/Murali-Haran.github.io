### initial processing --------------------------------------------------------
temp <- read.csv('http://www.stat.psu.edu/~mharan/380/data/tempGHCN/DailyTempSCE20022016.csv')
# look for missing values
all(is.na(temp$Measurement.Flag)) # should be TRUE
temp$Time.of.Observation[temp$Time.of.Observation == 9999.0] <- NA
summary(temp$Time.of.Observation)
# subset the data
tempSCE <- temp[,c("DATE","TMIN","TMAX","TOBS")]
names(tempSCE) <- c("date","tmin","tmax","tobs") # convenient to have names in lower case
# remove these (checked through documentation that this didn't seem like an issue)
tempSCE <- tempSCE[-c(1606, 5470, 5475), ]
# convert it into character strings
tempSCE$date <- as.character(tempSCE$date)
# can also use strptime, e.g. 
tempSCE$date <- as.POSIXct(tempSCE$date, format = "%Y%m%d")



### finding the monthly averages ----------------------------------------------
years <- as.character(2002:2016)
months <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
avg_monthly <- data.frame(date = rep(as.POSIXct(NA), 15*12), tobs = rep(NA, 15*12))
count <- 1

for(i in years)
{
  for(j in months)
  {
    date_char <- paste(i, j, '01', sep = '-')
    rows <- which(format(tempSCE$date, format = '%Y') == i &
                    format(tempSCE$date, format = '%m') == j)
    temp_sub <- tempSCE[rows, ]
    
    avg_monthly$date[count] <- as.POSIXct(date_char)
    avg_monthly$tobs[count] <- mean(temp_sub$tobs)
    
    count <- count + 1
  }
}

plot(avg_monthly$date, avg_monthly$tobs, type = 'l', main = 'Average temperature by month, State College', xlab = 'Date', ylab = 'Temperature (F)')
