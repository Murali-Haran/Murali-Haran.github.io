####################################
## examples of regular expressions
## and basic text processing
####################################


cNames = c("Dewitt County", 
           "Lac qui Parle County", 
           "St. John the Baptist Parish", 
           "Stone County",
           "Lewis & Clark County")


tolower(cNames)
cN2 = tolower(cNames)

nchar(cN2)


substr(cN2, start = rep(1, length(cN2)), 
       stop = (nchar(cN2) - 6))
[1] "dewitt "               "lac qui parle "       
[3] "st. john the baptist " "stone "               
[5] "lewis & clark " 

substr(cN2, start = 1, stop = (nchar(cN2) - 7))
[1] "dewitt"               "lac qui parle"       
[3] "st. john the baptist" "stone"               
[5] "lewis & clark" 

cN3 = substr(cN2, start = 1, stop = (nchar(cN2) - 7))

splits = strsplit(cN3, "")

splits[[3]][ !(splits[[3]] == ".")]
splits[[1]][ !(splits[[1]] == ".")]

x = splits[[1]][ !(splits[[1]] == ".")]
paste(x, collapse ="")

sapply(splits, function(string) {
    paste(string[ !(string == ".") ], collapse = "")
  }
)

cN4 = sapply(splits, function(string) {
  tmp = string[ !(string == ".") ]
  amp = which(tmp == "&")
  if (!(length(amp) == 0)) {
    tmp[amp] = "and"
  } 
  paste(tmp, collapse = "")
}
)

xx = c("Re: 90 days", "Fancy rep1!c@ted watches", "It's me")
grep("[[:alpha:]][[:digit:][:punct:]][[:alpha:]]", xx)
newX = gsub("'", "", xx)
grep("[[:alpha:]][[:digit:][:punct:]][[:alpha:]]", newX)

gregexpr("[[:alpha:]][[:digit:][:punct:]][[:alpha:]]", newX)

gregexpr("[[:alpha:]][[:digit:][:punct:]]+[[:alpha:]]", newX)

##########################################
##########################################

cNames = c("Dewitt County", 
           "Lac qui Parle County", 
           "St. John the Baptist Parish", 
           "Stone County",
           "Lewis & Clark County")


tolower(cNames)
#[1] "dewitt county"               "lac qui parle county"       
#[3] "st. john the baptist parish" "stone county"               
#[5] "lewis & clark county"
cN2 = tolower(cNames)

nchar(cN2)
#[1] 13 20 27 12 20

substr(cN2, start = rep(1, length(cN2)), 
       stop = (nchar(cN2) - 6))
#[1] "dewitt "               "lac qui parle "       
#[3] "st. john the baptist " "stone "               
#[5] "lewis & clark " 

substr(cN2, start = 1, stop = (nchar(cN2) - 7))
#[1] "dewitt"               "lac qui parle"       
#[3] "st. john the baptist" "stone"               
#[5] "lewis & clark" 

cN3 = substr(cN2, start = 1, stop = (nchar(cN2) - 7))

splits = strsplit(cN3, "")

class(splits)
#[1] "list"
length(splits)
#[1] 5
splits[[1]]
#[1] "d" "e" "w" "i" "t" "t"

splits[[3]][ splits[[3]] != "." ]
splits[[1]][ splits[[1]] != "." ]

x = splits[[1]][ splits[[1]] != "." ]
paste(x, collapse ="")
#[1] "dewitt"

sapply(splits, function(string) {
    paste(string[ string != "." ], collapse = "")
  }
)
#[1] "dewitt"              "lac qui parle"      
#[3] "st john the baptist" "stone"              
#[5] "lewis & clark" 

cN4 = sapply(splits, function(string) {
  tmp = string[ string != "." ]
  amp = which(tmp == "&")
  if (!(length(amp) == 0)) {
    tmp[amp] = "and"
  } 
  paste(tmp, collapse = "")
}
)

cN4
#[1] "dewitt"              "lac qui parle"      
#[3] "st john the baptist" "stone"              
#[5] "lewis and clark"   

#### using sub 
sub("&", "and", cNames)
sub(" County", "", cNames)
sub("Parish","", cNames[3])
sub("Parish","", cNames[2])
sub("Parish","", cNames[2],ignore.case = TRUE)
sub("[pP]arish","", cNames[2])

##########################################
##########################################

wl1 = '169.237.46.168 - - [26/Jan/2014:10:47:58 -0800] "GET /stat141/Winter04 HTTP/1.1" 301 328 
"http://anson.ucdavis.edu/courses/" "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0; .NET CLR 1.1.4322)"'


xtractDate = strsplit(wl1, split = " ")[[1]][4]

xtractDate1 = substr(xtractDate, start = 2, stop = nchar(xtractDate))

xtractDate2 = strsplit(xtractDate1, split = "/")

day = xtractDate2[[1]][1]
mon = xtractDate2[[1]][2]
yr = strsplit(xtractDate2[[1]][3], ":")[[1]][1]

## An Alternative that use regular expressions
v = strsplit(wl1, "\\[|/|:")[[1]][2:4]

v = strsplit(wl1, "[[/:]")[[1]][2:4]

# Other alternatives that are less clean
x = gsub(".*\\[", "", wl1)
x = gsub(":.*", "", x)
x = unlist(strsplit(x, split = "/"))


y = gsub(".*\\[([^:]*).*", "\\1", wl1)
y = unlist(strsplit(y, split = "/"))


locs = regexpr("\\[[^:]*", wl1)
z = substr(wl1, start = locs + 1, 
           stop = locs + attributes(locs)$match.length - 1)
z = unlist(strsplit(z, split = "/"))

####################################
## 
####################################
foo = c("", "HELP!", "Hi", "123","Boohoo","hhhellooo","hoover","ox")
grep("^[H]", foo)
grep("H", foo)
grep("[H]", foo)
grep("[Hh]", foo)
grep("[Hh][Eo]", foo)
grep("[Hh][Eo]", foo)
grep("[H?][Eo]", foo)
grep("[[Hh]?][Eo]", foo)
grep("[[Hh]*][Eo]", foo)
grep("[H*h*][Eo]", foo)
grep("[H?h?][Eo]", foo)
