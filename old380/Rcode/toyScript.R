### can call this script from the command line and
### get arguments as follows: 
### Rscript --vanilla toyScript.R iris.txt out.txt
args = commandArgs(trailingOnly=TRUE)
cat(args[1],"\n")
cat(args[2],"\n")
