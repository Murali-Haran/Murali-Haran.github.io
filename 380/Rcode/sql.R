######## Use DBI package #######
## Example code from help page for `dbConnect`
##library(DBI)
# Initialize a temporary in memory database and copy a data.frame into it
##con <- dbConnect(RSQLite::SQLite(), ":memory:")

######## Or use RSQLite package #######
library(RSQLite) # load the library
## set path to DB file first (if necessary)
drv = dbDriver("SQLite") ## load database driver SQLite
con = dbConnect(drv, dbname="USArrests")
data(USArrests)
##dbWriteTable(con, "USArrests", USArrests)
dbListTables(con)

# Fetch all query results into a data frame:
dbGetQuery(con, "SELECT * FROM USArrests")

# Or do it in batches
rs <- dbSendQuery(con, "SELECT * FROM USArrests")
d1 <- dbFetch(rs, n = 10)      # extract data in chunks of 10 rows
dbHasCompleted(rs)
d2 <- dbFetch(rs, n = -1)      # extract all remaining data
dbHasCompleted(rs)
dbClearResult(rs)

# clean up
dbDisconnect(con)

############################################################
###install.packages("RMySQL", dependencies=TRUE, repos='http://cran.rstudio.com/')
############################################################

library(RSQLite) # load the library
## set path to DB file first (if necessary)
drv = dbDriver("SQLite") ## load database driver SQLite
con = dbConnect(drv, dbname="lean_imdbpy_2010.db") # open connection to this database

dbListTables(con)  # this shows a list of all the tables in the database
##  [1] "aka_name2"       "aka_title2"      "cast_info2"      "info_type"      
##  [5] "keyword2"        "kind_type"       "movie_info2"     "movie_info_idx2"
##  [9] "movie_keyword2"  "name2"           "person_info2"    "role_type"      
## [13] "sqlite_sequence" "title2"         

dbListFields(con, "cast_info2")
## [1] "id"             "person_id"      "movie_id"       "person_role_id"
## [5] "note"           "nr_order"       "role_id"


### get everything from name2 
## nameInfo = dbGetQuery(con, "SELECT * FROM name2;")
## ## look up cast information
## castInfo=dbListFields(con, "cast_info2")
rs <- dbSendQuery(con, "SELECT * FROM name2")
d1 <- dbFetch(rs, n = 10)
dbClearResult(rs)

### count the number of people in the database 
countPeople = dbGetQuery(con, "SELECT count(*) FROM name2;")

### count the number of women in the database 
countFemale = dbGetQuery(con, "SELECT count(*) FROM name2 WHERE gender = 'f';")

### proportion of women: 
countFemale/countPeople
## 0.2095444

### among females in database, 
female = dbGetQuery(con, "SELECT * FROM name2 WHERE gender = 'f';")
dbGetQuery(con, "SELECT * FROM name2 LIMIT 3;") # limit to just 3 rows

cF = dbGetQuery(con, "SELECT COUNT(DISTINCT person_id) FROM cast_info2 WHERE role_id = 2;")

cP = dbGetQuery(con, "SELECT COUNT(DISTINCT person_id) FROM cast_info2 WHERE role_id = 1 OR role_id = 2;")

numMovies = dbGetQuery(con, "SELECT count(*) FROM title2 WHERE kind_id = 1;") ## count the number of movies (not tv shows)
## > numMovies
##   count(*)
## 1   292918

###dbGetQuery(con, "SELECT * FROM movie_info2 WHERE info_type_id = 3 LIMIT 3;")
dbGetQuery(con, "SELECT * FROM movie_info2 WHERE info = 'Horror' LIMIT 3;")
dbGetQuery(con, "SELECT count(*) FROM movie_info2 WHERE info = 'Horror';")

### More complicated query
## List top 20 actors with their names and count of movies
## store in data.frame called top20 with columns: id ("pid"), name ("name") and count ("numM")
popAct = dbGetQuery(con, "SELECT C.person_id AS pid, N.name AS name, COUNT(*) as numM 
                    FROM cast_info2 AS C
                    INNER JOIN title2 AS T
                    ON  C.movie_id = T.id
                    INNER JOIN name2 AS N
                    ON C.person_id = N.id
                    WHERE C.role_id = 1 AND T.kind_id = 1 AND N.gender = 'm'
                    GROUP BY C.person_id ORDER BY numM DESC LIMIT 20;")

########## END session
dbDisconnect(con) # should return TRUE
dbUnloadDriver(drv)# should return TRUE
