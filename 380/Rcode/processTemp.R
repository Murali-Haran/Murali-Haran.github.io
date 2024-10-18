############################################################
## Instructions for downloading temperature data
## NOAA National Centers for Environmental Information
## https://www.ncdc.noaa.gov/cdo-web/
## Click: browse datasets--> Daily Summaries
## Now: select Daily Summaries, appropriate Data Range,
## Search for ZIP codes, Enter: 16801 for search word
## Click: add to cart (next to State College, PA 16801 etc.)
## Click to view cart item. 
## Then: select output format (2nd one) Custom GHCN-Daily CSV
## Select date range: Jan 18, 2002 to Jan 17, 2017
## Click Apply, then continue
## Click: Station name, geographic location, include data flags
## Units: Standard;  Select: Air temperature (TMAX, TMIN, TOBS)
## Click Continue
## Enter email addresses, then click Submit
############################################################

#############################################################
### read the data 
##temp=read.table("DailyTempSCE2016.csv", header=TRUE,sep=",")
#temp=read.table("DailyTempSCE20022016.csv", header=TRUE,sep=",")
temp=read.table("http://www.stat.psu.edu/~mharan/380/data/tempGHCN/DailyTempSCE20022016.csv", header=TRUE,sep=",")
str(temp) ##
dim(temp)
##[1] 5478   21
#############################################################

#############################################################
## preliminary cleaning
#############################################################

all(is.na(temp$Measurement.flag))
### turning 9999 into NAs
temp$Time.of.Observation[temp$Time.of.Observation==9999.0]=NA 
summary(temp$Time.of.Observation)
   ## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   ##  700     700     700     700     700     700       2 

## subset the data
tempSCE=temp[,c("DATE","TMIN","TMAX","TOBS")]
names(tempSCE)=c("date","tmin","tmax","tobs") # convenient to have names in lower case
str(tempSCE)
##dimensions: 5478    4

## check class of all variables (vectors) at once using sapply
sapply(tempSCE, class)
##[1] FALSE

## look at all summaries
apply(tempSCE,2, summary)

## it appears that some vectors have -9999.00 values, which indicates missingness/problems
tempSCE[(tempSCE$tmin==-9999),]
tempSCE[(tempSCE$tmax==-9999),]
tempSCE[(tempSCE$tobs==-9999),]

## identified via above: rows 1606, 5470, 5475
## remove these (checked through documentation that this didn't seem like an issue)
tempSCE=tempSCE[-c(1606, 5470, 5475), ]
dim(tempSCE)

#############################################################
## DATES
## Convert the month and day variables into a POSIXct formatted
## variable. Include the year in the format. Begin by creating a string
## of the year, month, and day, where each element is separated by a
## dash. Then use this string to create a POSIXct formatted date
# variable. The help file of strptime() is useful here.
#############################################################

### process dates into something standard?
class(tempSCE$date)
## [1] "integer"
## convert it into character strings
tempSCE$date=as.character(tempSCE$date)
##tempSCE$date=as.POSIXct(foo, format="%Y%m%d")
### can also use strptime, e.g. 
tempSCE$date=strptime(tempSCE$date, format="%Y%m%d")
##tempSCE$date=as.POSIXct(foo, format="%Y%m%d")
class(tempSCE$date)

### look at differences from day to day
diff(strptime(tempSCE$date[1:50],format="%Y%m%d"))
### Extract year, date or time from this format
format(tempSCE$date[1:50],'%Y')
## compare to see the date
format(tempSCE$date[1:3],'%Y')==c("2002", "2003", "2004")

### how can we use this to figure out the average for a year?

### SUMMARY STATISTICS
## look at histograms, density plots?
## overlay tmin, tmax, tobs distributions
## overlay time series of these 3 data sets

### subset the data, look at one date per month
### create monthly data

### some plotting
plot(seq(1,length(tempSCE$tobs)), tempSCE$tobs)

###as.POSIXct()
