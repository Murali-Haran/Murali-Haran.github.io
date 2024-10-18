
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
