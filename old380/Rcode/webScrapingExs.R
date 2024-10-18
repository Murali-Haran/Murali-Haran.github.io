# India stock exchange

uIn = "http://www.nseindia.com/archives/nsccl/volt/CMVOLT_07062012.CSV"
d1 = read.csv(url(uIn))
#Error in open.connection(file, "rt") : cannot open the connection
#In addition: Warning message:
#In open.connection(file, "rt") :
#  cannot open URL 'http://www.nseindia.com/archives/nsccl/volt/CMVOLT_07062012.CSV': 
# HTTP status was '403 Forbidden'


d2 = getURL(uIn, verbose = TRUE)
writeLines(d2, "indiaNSE403.txt")
#*   Trying 23.7.66.103...
#* Connected to www.nseindia.com (127.0.0.1) port 80 (#0)
#  > GET /archives/nsccl/volt/CMVOLT_07062012.CSV HTTP/1.1
#  Host: www.nseindia.com
#  Accept: */*
    
#    < HTTP/1.1 403 Forbidden
#  < Server: AkamaiGHost
#  < Mime-Version: 1.0
#  < Content-Type: text/html
#  < Content-Length: 325
#  < Expires: Mon, 28 Nov 2016 16:31:32 GMT
#  < Date: Mon, 28 Nov 2016 16:31:32 GMT
#  < Connection: close
#  < 
#    * Closing connection 0
#  Error: Forbidden


d2 = getURL(uIn, useragent = "R", verbose = TRUE)
#*   Trying 23.7.66.103...
#* Connected to www.nseindia.com (127.0.0.1) port 80 (#0)
#> GET /archives/nsccl/volt/CMVOLT_07062012.CSV HTTP/1.1
#Host: www.nseindia.com
#User-Agent: R
#Accept: */*
#    
#< HTTP/1.1 302 Moved Temporarily
#< Server: AkamaiGHost
#< Content-Length: 0
#< Location: https://www.nseindia.com/archives/nsccl/volt/CMVOLT_07062012.CSV
#< Date: Mon, 28 Nov 2016 16:33:23 GMT
#< Connection: keep-alive
#< 
#* Connection #0 to host www.nseindia.com left intact

d = getURL(uIn, useragent = "R", followlocation = TRUE, verbose = TRUE)  
head(read.csv(textConnection(d)))

#dd = rawToChar(d)

u8Feb = "http://www.nseindia.com/content/ historical/EQUITIES/2012/FEB/cm08FEB2012bhav.csv.zip"
z = getURLContent(u8Feb, useragent = "R", followlocation = TRUE)

library(Rcompression)
txt = zipArchive(z)[[1]]

# scraping an html table
library(XML)
library(RCurl)

wP = "https://en.wikipedia.org/wiki/United_States_presidential_election_in_Virginia,_2004"
hC = getURLContent(wP)

pdoc = htmlParse(hC)
pRoot = xmlRoot(pdoc)
cTable = getNodeSet(pRoot, 
      "//table//td/a[@title='Accomack County, Virginia']/../../..")

xmlSize(cTable[[1]])

wTs = readHTMLTable(hC)
sapply(wTs, dim)
wC04 = readHTMLTable(hC, which = 9, 
                     colClasses = c("character",
                                    "Percent", "FormattedInteger",
                                    "Percent", "FormattedInteger",
                                    "Percent", "FormattedInteger"),
                     stringsAsFactors = FALSE)




wikiPage = "https://en.wikipedia.org/wiki/1500_metres_world_record_progression"
htmlContent = getURL(wikiPage)


pageDoc = xmlParse(htmlContent)
pageRoot = xmlRoot(pageDoc)
pageBody = pageRoot[["body"]]

tables = getNodeSet(pageRoot, "//table")
length(tables)
xmlSApply(tables, xmlSize)

iaafMen = tables[[2]]
iaafMen[[1]]
iaafMen[[2]]
xmlValue(iaafMen[[2]])

result = readHTMLTable(iaafMen, stringsAsFactors = FALSE)
head(result)

#  convert

tempTime = as.POSIXlt(result$Time, format = "%M:%OS")
runTime = (tempTime$min * 60) + tempTime$sec
athlete = sub("[[:blank:]]\\(.{3}\\)$", "", result$Athlete)
ctry = sub(".*[[:blank:]]\\((.{3})\\)$", "\\1", result$Athlete)
recordSet = as.Date(result$Date, format = "%Y-%m-%d")

finalTable = data.frame(runTime, athlete, ctry, recordSet)

library(ggplot2)
ggplot(data = rbind(finalTable[c(1,4)], 
                    data.frame(runTime = finalTable$runTime[nrow(finalTable)],
                               recordSet= as.Date("2016-11-30", format = "%Y-%m-%d"))),
       aes(x = recordSet, y = runTime)) +
  geom_step(direction = "hv") +
  labs(x = "Year", y = "Time (sec)", 
       title = "World Records in Men’s 1500 meter") +
  lims(x = as.Date(c("1912-01-01", "2016-11-30"), format = "%Y-%m-%d"))


#################
## EXAMPLE OF GET
#GOOGLE example

# Go to google.com and look at the source. Find the form and present it.

# Type in a search query and hit return
# Examine the resulting url
#"https://www.google.com/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=17+only+truly+random+number"

query = 
  "https://www.google.com/search?q=17+only+truly+random+number&sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8"
get17Query = getURL(query)
writeLines(get17Query, "17query.html")

# copy and paste the url into a separate tab in the  browser 

#identify the fields in the GET, 



get17Info = getForm("https://www.google.com/search",
                    q = "17+only+truly+random+number",
                    sourceid = "chrome-instant",
                    ion = "1",
                    espv = "2",
                    ie = "UTF-8")

writeLines(get17Info, "get17info.html")

get17V3 = getForm("https://www.google.com/",
                   q="17+only+truly+random+number")

writeLines(get17V3, "get17V3.html")

links = getHTMLLinks(get17Info)
# Many of these are not the links we are after

### SECOND GET EXAMPLE 
# Kaggle with multiple pages

#Visit
#https://www.kaggle.com/jobs

uK = "https://www.kaggle.com/jobs"

#View source - identify the form and the GET method
#  <input type="text" name="q" class="search"/>
#  <input type="submit" value="" class="search"/>
#  <input type="hidden" name="sitesearch" value="www.kaggle.com/jobs/"/>
  
# This does not work
resK = getForm(uK, q = "r+python", sitesearch = "www.kaggle.com%2Fjobs%2F",
               submit = "")

#https://www.google.com/search?q=r+python&sitesearch=www.kaggle.com%2Fjobs%2F

# This does not work either
uG = "https://www.google.com"
resK = getForm(uG, q = "r+python", 
               sitesearch = "www.kaggle.com/jobs/")

# Using getURL does work...
resK2 = getURL("https://www.google.com/search?q=r+python&sitesearch=www.kaggle.com%2Fjobs%2F")

resKdoc = htmlParse(resK2)
resKRoot = xmlRoot(resKdoc)
resKlinks = getNodeSet(resKRoot, "//h3/a[@href]")
xmlSApply(resKlinks, xmlGetAttr, "href")

# To get additional pages,

nPLink = "https://www.google.com/search?q=r+python+site:www.kaggle.com%2Fjobs%2&start=10&sa=N"
nP = getURL(nPLink)
nPdoc = htmlParse(nP)
nProot = xmlRoot(nPdoc)
nPlinks = getNodeSet(nProot, "//li/a[@href]")
xmlSApply(nPlinks, xmlGetAttr, "href")



# Try it with "R python" as the query  
#https://www.google.com/search?q=r+python&sitesearch=www.kaggle.com%2Fjobs%2F

# Notice the format of the URL
#https://www.google.com/search?q=r+python+site:www.kaggle.com/jobs/&ei=Rb44WO_RH4mMjwPlxaSABQ&start=10&sa=N

# Task - 
#  1. get links in a page
#  2. get the next page
# 3. recognize the end


search1 = 
"https://www.google.com/search?q=r+python&sitesearch=www.kaggle.com%2Fjobs%2F"

searchC = 
"https://www.google.com/search?q=r+python+site:www.kaggle.com/jobs/&ei=Rb44WO_RH4mMjwPlxaSABQ&start=10&sa=N"


#### GAS EXAMPLE


library(XML)
u = "http://www.energy.ca.gov/almanac/transportation_data/gasoline/margins/"
tbls = readHTMLTable(u, stringsAsFactors = FALSE)

gasPage = getURL(u)
colTypes = c("character", "NULL", rep("Currency", 8), "NULL",
             rep("Currency", 8))
tbl2 = readHTMLTable(gasPage, which = 1, colClasses = colTypes)


length(tbls)

tbl = tbls[[1]]

dim(tbl)

head(tbl)

prices = lapply(tbl[-c(1, 2, 11)], 
                function(vec) as.numeric(gsub("\\$", "", vec)) )

priceDF = as.data.frame(do.call(cbind, prices))
names(priceDF) = c(paste("Branded", 1:8, sep =""), 
                   paste("Unbranded", 1:8, sep =""))
priceDF$wk = as.Date(tbl[[1]], "%b %d" )
priceDF = priceDF[-(1:3), ]




## SKIP THIS ERROR
colTypes = c("character", "NULL", rep("Currency", 8), "NULL",
            rep("Currency", 8))
tbl2 = readHTMLTable(u, which = 1, colClasses = colTypes)

library(RCurl)
txt = postForm(u, year = "2013", newYear = 'Get different year')
gas13 = readHTMLTable(txt, which = 1, stringsAsFactors = FALSE)
head(gas13)

#############
library(twitteR)

consumer_key = "7P5ablM7SlCR1cwGMudx9quSR"
consumer_secret = "UycjC6IYtTQP6LZyNFuIxw91vfPd4bIHCPJpfKdZbf8uCo3Q0c"
access_token = "368404561-7t6gw4NaRIt5FycPyPkif2LM9FnBvfQUlkEb3d6x"
access_secret = "7LpQsRV8RYdooG7s5Z4RPcW9AzCe1LvKhUTe72w5KsT3e"

setup_twitter_oauth(consumer_key, consumer_secret, 
                    access_token, access_secret)

tweets_sanders = searchTwitter('@BernieSanders', n = 1500)


trump_tweets = userTimeline("realDonaldTrump", n = 3200)
trump_tweets_df <- twListToDF(trump_tweets)

# In future
setup_twitter_oauth(getOption("twitter_consumer_key"),
                    getOption("twitter_consumer_secret"),
                    getOption("twitter_access_token"),
                    getOption("twitter_access_token_secret"))


