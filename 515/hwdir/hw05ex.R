##################################################
## display 100 events uniformly distributed
## in time over the interval (0,50)
##################################################

simEvents = runif(10, 0, 50) # simulate 10 events uniformly over (0,50)
plot(cbind(simEvents, rep(1,length(simEvents))), xlim=c(0,50), cex=1, pch="|", main="Events from a Uniform process", xlab="t (time)") # plot the event times

