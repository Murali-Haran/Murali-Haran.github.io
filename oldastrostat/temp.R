###
boo <- seq(1,14258,by=100)
diffs <- c()
for (i in 1:(length(boo)-2))
{
  newdiff <- mean(coup554$Ener[boo[i]:boo[i+1]])-mean(coup554$Ener[(boo[i+1]+1):boo[i+2]])
  cat(i,newdiff,mean(coup554$Ener[boo[i]:boo[i+1]]),mean(coup554$Ener[(boo[i+1]+1):boo[i+2]]),"\n")
  diffs <- c(diffs,newdiff)
}
27 0.4875762 3.822376 3.3348
ts.plot(coup554$Ener[boo[27]:boo[29]])


coup554$Ener
coup263$Ener
