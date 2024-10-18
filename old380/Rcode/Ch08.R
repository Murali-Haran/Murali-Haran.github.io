## Installation help for XML package
## install.packages("XML", dependencies=TRUE, repos='http://cran.rstudio.com/')

######################################################################
## Regular expression: pattern that describes a set of strings
## Detailed information on regular expressions is under help(regex)
######################################################################

########################
## Match Literals
########################
eg = "The cad hid his coat. Scat!"
grep("cat",eg)
grepl("cat",eg)
gsub("cat","BOO",eg)

################################################
## Modifiers and metacharacters
################################################
## Want to search for Re: at the beginning of
## a string, also allowing for empty space
eg = c("It is all about Re:", "Re: Statistical topics","   Re: yesterday")
grep("Re",eg)
## to force it to be at the beginning, need
## modifier, i.e. caret (^)
grep("^Re",eg)
grepl("^Re",eg) # return booleans 
## force to be at beginning (^), but allow for as many blanks as you like
## with a <blank> followed by *.
grep("^ *Re",eg) 
## If you want to find and delete "Re:" at the beginning, allowing for spaces
## before and after
gsub("^ *Re: *","",eg)

################################################
## Note:
## to specify one of these meta characters as a literal in a pattern,
## it needs to be preceded with two backslashes in R
################################################

## search for cat, a separate word, only, not cat as part of some
## other word
eg = "Feed the cat or there will be a catastrophe"
gsub("cat","dog",eg) # matches any cat
gsub("\\<cat\\>","dog",eg) # matches only cat as a separate word: \< is a metacharacter

eg = "Feed the <cat or there will be a catastrophe"
gsub("<cat","dog",eg) # matches any <cat (treats < as a literal since < is not a metacharacter)
eg = "Feed the ^cat or there will be a catastrophe"
gsub("^cat","dog",eg) 
gsub("\\^cat","dog",eg) # matches ^cat (treats ^ as a literal since it is preceded by \\)

eg = "Feed the cat or caat or caaat"
gsub("a+","dog",eg)
gsub("a?","dog",eg)

################################################
## EQUIVALENT CHARACTERS, CHARACTER CLASSES
## anything between [] are considered equivalent
## - : specifies a range, e.g. a-d is a or b or c or d
## ^ : exclude anything that follows it
################################################
eg="The cad hid his coat and coot. Scat!"
gregexpr("ca[td]",eg) # matches cat or cad (t and d are equivalent)
gsub("ca[td]","dog",eg) # matches cat or cad (t and d are equivalent)
gsub("c[ao]+[td]","dog",eg) # matches c then (a or o) one or more times then t or d (exactly once)

### what if we want to extract just the user id before @psu.edu 
eg = "muh10@psu.edu ep2m@psu.edu john.ensley@psu.edu"
spliteg = strsplit(eg," ")
eg = "muh10@psu.edu ep2m@psu.edu/john.ensley@psu.edu"
spliteg = strsplit(eg,"[/ ]") # split on either blank or / 
spliteg[[1]][2]
spliteg=unlist(spliteg)
sapply(spliteg, function(x) sub("@.*","",x))

### search for a digit or an underscore at the end of the username of an email address
emaileg=c("depchairs03-04@uclink.berkeley.edu","Sarah Lang <sarah_@hotmail.com>","John the 2nd <nolan@hotmail.com>","John Nolan <nolan12345@hotmail.com>","Sarah_Lang <sarah@hotmail.com>", ":@hotmail.com","d@yahoo.com", "t@gmail.com")
grep("[[:digit:]_]@",emaileg)
grep("[:digit:_]@",emaileg) # how is this different? It allows for : or d or i or ... _ followed by @

eg=c("Today","Wednesday","Yesterday","Friday","Sunday")
grep("(Sun|Mon|Tues|Wednes|Thurs|Fri|Satur)day", eg)

##################################################
## Example of extracting info from a weblog
## From Chapter 8 of book
## Desired output: IP address, date, time, filename,
## status, number of bytes transferred
##################################################
weblogEg='193.188.97.151 - - [29/Dec/2003:06:36:18 -0600] "GET /logo.html  HTTP/1.1" 200 244 foobar baz quux woof'

##################################################
## we want to produce the following:
## 193.188.97.151, 29/Dec/2003, 06:36:18, /logo.html, 200, 244

## Format: IP address comes first, followed by - -
## [date:time] "GET (or POST) filename HTTP..."
## status number number of bytes junk
## Use pattern matching to extract information
## sub patterns that appear within parentheses
## automatically labeled sequentially in the pattern 
## E.g. by writing \\1 or \\2 etc. we can extract the
## first second or third subexpression corresponding
## to the wildcard (.*)
##################################################
## to get just the IP address and date and time and junk (everything before GET)
gsub('(.*)"GET .*  HTTP.*', '\\1', weblogEg)

## to get just the IP address alone
gsub('(.*) - - \\[.*', '\\1', weblogEg)

## to get just the filename
gsub('.*"GET (.*)  HTTP.*', '\\1', weblogEg)

##################################################
## We could extract the entries one by one 
## but what if we wanted to do all the extraction at the same time?
## Pattern match multiple at once
##################################################
## to get the IP address and filename at the same time
gsub('(.*) - - \\[.*"GET (.*) HTTP.*', '\\1, \\2', weblogEg)

## now do all at once by constructing a (big!) pattern
##weblogPattern='(.*) - - \\[(.*):(.*) -.*\\] "GET (.*) HTTP(.*)'
##gsub(weblogPattern, '\\1, \\2, \\3', weblogEg)
## Above doesn't quite work. Why not?

weblogPattern='(.*) - - \\[(.*):([0-9]{2}:[0-9]{2}:[0-9]{2}).*\\] "GET (.*) HTTP(.*)'
gsub(weblogPattern, '\\1, \\2, \\3', weblogEg)

##weblogPattern = '(.*) - - \\[(.*):([0-9]{2}:[0-9]{2}:[0-9]{2}).*\\] "(GET|POST) (.*) (HTTP|FTP)(/1.[01])?" ([0-9]+) (-|[0-9]+).*'
weblogPattern = '(.*) - - \\[(.*):([0-9]{2}:[0-9]{2}:[0-9]{2}).*\\] "(GET) (.*) (HTTP)(/1.[01])?" ([0-9]+) (-|[0-9]+).*'

## '(.*) - - \\[(.*):([0-9]{2}:[0-9]{2}:[0-9]{2}).*\\] "GET (.*) (HTTP|FTP)(/1.[01])?" ([0-9]+) (-|[0-9]+).*'
gsub(weblogPattern, '\\1, \\2, \\3, \\5, \\8, \\9', weblogEg)


