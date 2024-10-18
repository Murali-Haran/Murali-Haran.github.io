binCalc=function(x){
  x=as.character(x)
  binVec=as.numeric(strsplit(x,"")[[1]])
  n=length(binVec)
  tot=0
  for (i in binVec)
  {
    n=n-1
    tot=tot+i*2^n
  }
  return(tot)
}

