
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
