################################################################
### (0) Read in the data
### (1) extract relevant columns only
### (2) make sure no errors (every element in vector Measurement.Flag==NA)
### (3) fix missing vals: 9999.0 -> NA
################################################################
tempSCE <- read.csv('http://www.stat.psu.edu/~mharan/380/data/tempGHCN/DailyTempSCE20022016.csv')
## subset the data: extract relevant columns
tempSCE <- tempSCE[,c("DATE","TMIN","TMAX","TOBS","Time.of.Observation")]
names(tempSCE) <- c("date","tmin","tmax","tobs","timeObs") # convenient to have names in lower case

### Check that there is no major problem
all(is.na(temp$Measurement.Flag)) # should be TRUE
## identify missing data
## Recall that missing values are denoted by 9999.0 or =9999.0
which(tempSCE$tmin==-9999.00)
##[1] 5470
which(tempSCE$tmax==-9999.00)
##[1] 1606 5475
which(tempSCE$tobs==-9999.00)
##integer(0)
which(tempSCE$timeObs==9999.0)
##[1] 1606 5475

## as in previous homework, remove rows with missing values
## but do it in a more automated fashion (don't paste in actual row numbers)
missingRows=union(union(which(tempSCE$tmin==-9999.00), which(tempSCE$tmax==-9999.00)),which(tempSCE$timeObs==9999.0))
tempSCE <- tempSCE[-missingRows,]

## take a look at what the date variable looks like
head(tempSCE$date,20)
tail(tempSCE$date,20)

# convert it into character strings
tempSCE$date <- as.character(tempSCE$date)
# can also use strptime, e.g. 
tempSCE$date <- as.POSIXct(tempSCE$date, format = "%Y%m%d")

### Now what do we do?
### Create a new data frame with MONTH YEAR AVGTEMPS

