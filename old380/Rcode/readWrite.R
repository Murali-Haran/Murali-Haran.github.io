########################
## readLines and
## writeLines
## read and write lines
## by default from stdin/stdout
########################

## e.g. read spam file
email = readLines('http://sites.stat.psu.edu/~mharan/380/data/spam/spam/00001.7848dde101aa985090474a91ec93fcf0')
email[1]
## e.g. read spam file, but just the first 3 lines
email = readLines('http://sites.stat.psu.edu/~mharan/380/data/spam/spam/00001.7848dde101aa985090474a91ec93fcf0',n=3)
typeof(email)
length(email)

### writeLines writes out character vector to given connection
## default connection:  stdout (standard output: to the console)
writeLines(email)

### or: create a connection and write to it
trashFile = file("myTrash.txt", "w") ## open file connection in w = write mode
writeLines(email,trashFile)
close(trashFile) ## close connection (until you do this, reading/writing operations are incomplete)

### can re-open file connection
myFile = file("myTrash.txt", "r") ## open file connection in r = read mode
readLines(myFile,n=1)
close(myFile) ## close connection

### can create other types of connections, e.g. to a url
### can use other functions, e.g. scan to read from the url
emailURL = url('http://sites.stat.psu.edu/~mharan/380/data/spam/spam/00001.7848dde101aa985090474a91ec93fcf0',"r")
email2 = scan(emailURL, what="character")
email2 = paste(email2,collapse=" ")

### readline command (not readLines) can also be used to read stdin, with a prompt
foo = readline(prompt="Enter an integer: ")
