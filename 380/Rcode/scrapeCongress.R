http://community.amstat.org/blogs/zheyu-wang/2018/02/01/stat-tricks-of-the-month
http://blog.revolutionanalytics.com/2017/11/charts-in-280-chars.html

In what is rapidly becoming a series — cool things you can do with R in a tweet— Julia Silge demonstrates scraping the list of members of the US house of representatives on Wikipedia in just 5 R statements in her January 12, 2018 post.

Since Twitter munges the URL in the third line when you cut-and-paste, here's a plain-text version of Julia's code:

library(rvest)

library(tidyverse) 

h <- read_html("https://en.wikipedia.org/wiki/Current_members_of_the_United_States_House_of_Representatives") 

reps <- h %>%  html_node("#mw-content-text > div > table:nth-child(18)") %>%  html_table() 

reps <- reps[,c(1:2,4:9)] %>% as_tibble()
