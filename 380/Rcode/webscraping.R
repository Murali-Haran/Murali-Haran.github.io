#### install.packages("rvest", dependencies=TRUE, repos='http://cran.rstudio.com/')
#### bsaed on: https://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/
library(rvest)
nocountry = html("http://www.imdb.com/title/tt0477348/?ref_=nv_sr_1")
nocountry %>% 
  html_node("strong span") %>%
  html_text() %>%
  as.numeric()
lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()

fileLoc="http://www-958.ibm.com/software/data/cognos/manyeyes/datasets/olympic2012withgdp/versions/1.txt"
ctry = read.csv(fileLoc, skip = 1, sep = "\t", header = FALSE, colClasses = c("character", rep("numeric", 5),rep("character", 3)))
