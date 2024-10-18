
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
