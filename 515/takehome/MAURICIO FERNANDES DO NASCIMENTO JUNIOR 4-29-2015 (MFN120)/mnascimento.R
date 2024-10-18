###Some functions are big, so in order to debug you can use the debug function
###Usage debug(function), and than just run the function(the code used to run is provided)
###To stop the debug just use undebug(function)

###Indicator realizong diagnostics
DiagnosticFlag<-FALSE

###Packages and functions required to plots and tables
if(DiagnosticFlag){
  new.install<-function(x){
    if(!require(package = as.character(x), character.only = T)){
      install.packages(x,dep=T)
      if(!require(package = as.character(x), character.only = T, quietly = T)){stop(paste("Package ",x," could not be installed"))}
    }
    require(x, character.only = T)
  }
  new.install("xtable")
  new.install("ggplot2")
  new.install("plyr")
  new.install("reshape")
  new.install("GMCM")
  new.install("grid")
  new.install("gridExtra")
  thema<-theme_bw(base_size = 20) +
    theme(axis.title.x = element_text(size = 8, colour = "black"), 
          axis.text.x  = element_text(angle = 0, size = 8, colour = "black"),
          axis.title.y = element_text(size = 8, colour = "black"), 
          axis.text.y  = element_text(angle = 0, size = 8, colour = "black"),
          legend.text  = element_text(size = 8, colour = "black"), 
          legend.title = element_text(size = 8, colour = "black"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "steelblue", linetype = NULL),
          panel.grid.minor = element_line(colour = "steelblue", linetype = NULL),
          text = element_text(size = 8, colour = "black"),
          title =  element_text(size = 8, face = "bold"))
  
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    new.install("grid")
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  
  
}


####Functions from professor webpage
source("http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R")
source("http://sites.stat.psu.edu/~mharan/batchmeans.R")


#Posterior distributions, please se pdf file


#Function:  calculate the proportional of the posterior distribution of BETA0
#Inputs:    beta0=parameter of interest,prior.mean=prior mean of the parameter,prior.var=prior variance of the parameter,
#           beta1=value of beta1,lambda=value of lambda,sigma=values of sigma,Y=observed sample,X=observed X
#Output:    log of the proportional of the posterior

beta0.posterior<-function(beta0,prior.mean,prior.var,beta1,lambda,sigma,Y,X){
  log.lik <- dnorm(x = beta0,mean = prior.mean, sd = sqrt(prior.var), log = T) +
    sum(dexpgauss(x = Y, mu = beta0 + beta1*X, sigma = sigma, lambda = lambda, log = T))
  return(log.lik)
}


#Function:  calculate the proportional of the posterior distribution of BETA1
#Inputs:    beta1=parameter of interest,prior.mean=prior mean of the parameter,prior.var=prior variance of the parameter,
#           beta0=value of beta0,lambda=value of lambda,sigma=values of sigma,Y=observed sample,X=observed X
#Output:    log of the proportional of the posterior

beta1.posterior<-function(beta1,prior.mean,prior.var,beta0,lambda,sigma,Y,X){
  log.lik <- dnorm(x = beta1,mean = prior.mean, sd = sqrt(prior.var), log = T) +
    sum(dexpgauss(x = Y, mu = beta0 + beta1*X, sigma = sigma, lambda = lambda, log = T))
  return(log.lik)
}


#Function:  calculate the proportional of the posterior distribution of lambda
#Inputs:    lambda=parameter of interest,prior.shape=prior shape of the parameter,prior.scale=prior scale of the parameter,
#           beta1=value of beta1,beta0=value of beta0,sigma=values of sigma,Y=observed sample,X=observed X
#Output:    log of the proportional of the posterior

lambda.posterior<-function(lambda,prior.shape,prior.scale,beta1,beta0,sigma,Y,X){
  log.lik <- dgamma(x = lambda,shape = prior.shape,scale =  prior.scale,log = T) +
    sum(dexpgauss(x = Y, mu = beta0 + beta1*X, sigma = sigma, lambda = lambda, log = T))
  return(log.lik)
}





#Function:  generate samples from the posterior distribution
#Inputs:    N=size of the chain you are interested,burn=size of the burning,init=vector with initial values first element is beta0 second is beta1 and third is labda
#           prior.mean0=prior mean for beta0,prior.var0=prior variance for beta0,prior.mean1=prior mean for beta1,prior.var1=prior variance for beta1
#           propo.var0=proposed variance for beta0,propo.var1=proposed variance for beta1,prior.shape=prior shape for lambda,prior.scale=prior scale for lambda
#           propo.varL=proposed variance for lambda,X=observed X,Y=observed sample,sigma=value of sigma,thin=thining parameter
#Output:    list with two elements. first is the chain, second is the number of accepted samples.

mh.sampling <- function(N,burn,init,prior.mean0=NULL,prior.var0=NULL,prior.mean1=NULL,prior.var1=NULL,
                        propo.var0=NULL,propo.var1=NULL,
                        prior.shape=NULL,prior.scale=NULL,propo.varL=NULL,X,Y,sigma,thin){
  #Calculating the chain size and generating the matrix to store it
  n<-N*thin+burn
  samples <- matrix(data = NA_real_,nrow = n, ncol = 3)
  #Generating the vector to store the acceptance
  accept<-c(0,0,0)
  #assigning inital values
  samples[1,] <- init
  #verifying if the same values as the initial should be used during the entire chain
  if(is.null(prior.mean0))samples[,1]<-init[1]
  if(is.null(prior.mean1))samples[,2]<-init[2]
  if(is.null(prior.shape))samples[,3]<-init[3]
  
  #Code to question 1, verify if it is supposed to sample other than beta1
  if(is.null(prior.mean0)){
    for(i in 2:n){
      #generating proposed using random walk
      proposed<-rnorm(n = 1, mean = samples[i-1,2], sd = sqrt(propo.var1))
      #calculate the log of the acceptaing probability
      prob.calc <- beta1.posterior(beta1 = proposed,prior.mean = prior.mean1,prior.var = prior.var1,beta0 = samples[i,1],lambda = samples[i-1,3],sigma = sigma,Y = Y,X = X)-
        beta1.posterior(beta1 = samples[i-1,2],prior.mean = prior.mean1,prior.var = prior.var1,beta0 = samples[i,1],lambda = samples[i-1,3],sigma = sigma,Y = Y,X = X)
      #generating the log of the uniform
      u<-log(runif(n = 1))
      #deviding if keep the value or not
      if(u<=min(0,prob.calc)){samples[i,2]<-proposed;accept[2]<-accept[2]+1}
      if(u>min(0,prob.calc)){samples[i,2]<-samples[i-1,2]}
    }
  }
  
  
  #Code to question 2, verify if it is supposed to sample from all parameters
  if(!is.null(prior.mean0)){
    for(i in 2:n){
      
      #simulating from beta0
      
      #proposing a value using normal distribution with mean equal to last step and variance pre defined
      proposed<-rnorm(n = 1, mean = samples[i-1,1], sd = sqrt(propo.var0))
      #calculating the log of the accepting probability
      prob.calc <- beta0.posterior(beta0 = proposed,prior.mean = prior.mean0,prior.var = prior.var0,beta1 = samples[i-1,2],lambda = samples[i-1,3],sigma = sigma,Y = Y,X = X)-
        beta0.posterior(beta0 = samples[i-1,1],prior.mean = prior.mean0,prior.var = prior.var0,beta1 = samples[i-1,2],lambda = samples[i-1,3],sigma = sigma,Y = Y,X = X)
      #generate the log of a uniform dist
      u<-log(runif(n = 1))
      #decide if accept the new value or not
      if(u<=min(0,prob.calc)){samples[i,1]<-proposed;accept[1]<-accept[1]+1}
      if(u>min(0,prob.calc)){samples[i,1]<-samples[i-1,1]}
      
      
      #simulating from beta1
      
      #proposing a value using normal distribution with mean equal to last step and variance pre defined
      proposed<-rnorm(n = 1, mean = samples[i-1,2], sd = sqrt(propo.var1))
      #calculating the log of the accepting probability
      prob.calc <- beta1.posterior(beta1 = proposed,prior.mean = prior.mean1,prior.var = prior.var1,beta0 = samples[i,1],lambda = samples[i-1,3],sigma = sigma,Y = Y,X = X)-
        beta1.posterior(beta1 = samples[i-1,2],prior.mean = prior.mean1,prior.var = prior.var1,beta0 = samples[i,1],lambda = samples[i-1,3],sigma = sigma,Y = Y,X = X)
      #generate the log of a uniform dist
      u<-log(runif(n = 1))
      #decide if accept the new value or not
      if(u<=min(0,prob.calc)){samples[i,2]<-proposed;accept[2]<-accept[2]+1}
      if(u>min(0,prob.calc)){samples[i,2]<-samples[i-1,2]}
      
      
      #simulating from lambda
      
      #proposing a value using gamma distribution with mean equal to last step and variance pre defined
      proposed<-rgamma(n = 1, shape = (samples[i-1,3]^2)/propo.varL, scale = propo.varL/samples[i-1,3])
      #calculating the log of the accepting probability
      prob.calc <- lambda.posterior(lambda = proposed,prior.shape = prior.shape,prior.scale = prior.scale,beta0 = samples[i,1],beta1 = samples[i,2],sigma = sigma,Y = Y,X = X)-
        lambda.posterior(lambda = samples[i-1,3],prior.shape = prior.shape,prior.scale = prior.scale,beta0 = samples[i,1],beta1 = samples[i,2],sigma = sigma,Y = Y,X = X)+
        dgamma(x = samples[i-1,3], shape = (proposed^2)/propo.varL, scale = propo.varL/proposed)-
        dgamma(x = proposed, shape = (samples[i-1,3]^2)/propo.varL, scale = propo.varL/samples[i-1,3])
      #generate the log of a uniform dist
      u<-log(runif(n = 1))
      #decide if accept the new value or not
      if(u<=min(0,prob.calc)){samples[i,3]<-proposed;accept[3]<-accept[3]+1}
      if(u>min(0,prob.calc)){samples[i,3]<-samples[i-1,3]}
    }
  }
  
  
  #creat a data.frame with the values and the number of simulations
  MCMC<-data.frame(samples,"ID"=1:n)
  names(MCMC)<-c("Beta0","Beta1","Lambda","ID")
  #returning the sample and the accepted number 
  return(list("MCMC"=MCMC,"Accept"=accept))
  
  
}






########################################################
########################################################
########################################################
#Question 1

#reading data
dados1<-read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")
#Runing mcmc, take something like 8sec
teste<-mh.sampling(N = 30000,init = c(5,0,0.4),prior.mean1 = 0,prior.var1 = 100,propo.var1 = 0.5,
                   X = dados1$V1,Y = dados1$V2,sigma = 1,thin = 1,burn = 2000)
#removing the burning
mcchain<-teste[[1]][2001:nrow(teste[[1]]),]
#removing the thinning
mcchain2<-subset(mcchain,mcchain$ID%%1==0)#none is removed


#b Calculating the expected value and variance
bm(vals = mcchain2[,2])

#c Calculating the confidence interval
quantile(mcchain[,2],probs = c(0.025,0.975))


#d Plotting density need ggplot2
if(DiagnosticFlag){
  p<-ggplot(data = mcchain)
  p+geom_density(mapping = aes(x = Beta1),fill="tomato")+thema+labs(y = "Density", x = expression(beta[1]))
  
}



#e Verifing how much our mcmc is realiable

#generating 25 chains in different initial points
values<-sapply(X = 1:25,simplify = F, FUN = function(x){
    data.frame(mh.sampling(N = 30000,thin = 1,burn=2000,init = c(5,rnorm(1,0,10),0.4),prior.mean1 = 0,prior.var1 = 100,propo.var1 = 1,X = dados1$V1,Y = dados1$V2,sigma = 1)[[1]],"MC"=x)
})
values<-do.call(what = rbind,args = values)
#plotting the chains, expected values and MCMCse by iteractions need ggplot2 GMCM grid gridExtraa
if(DiagnosticFlag){
  p<-ggplot(data = values)
  p1<-p+geom_line(mapping = aes(x = ID,y = Beta1, group = MC, colour = factor(MC)),alpha = 0.2)+thema+
    theme(legend.position="none")+labs(title = "MCMC",y="")
  tau<-GMCM:::cummean(teste[[1]][,2])
  tau2<-apply(X = matrix(10:nrow(teste[[1]])),MARGIN = 1,FUN = function(x){
    bm(vals = teste[[1]][1:x,2])$se
  })
  plotar<-data.frame("MCMCse"=tau2,"Id"=10:nrow(teste[[1]]))
  p<-ggplot(data = plotar)
  p2<-p+geom_line(mapping = aes(x = Id,y = MCMCse))+thema
  
  plotar<-data.frame("Mean"=tau,"Id"=1:nrow(teste[[1]]))
  p<-ggplot(data = plotar)
  p3<-p+geom_line(mapping = aes(x = Id,y = Mean))+thema
  layout <- matrix(c(1,1,2,3),byrow=T,nrow=2)
  multiplot(plotlist = list(p1,p3,p2),layout = layout)
  
}





#Question 2

#reading data
dados2<-read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")
#Runing mcmc, take something like 8sec
teste2<-mh.sampling(N = 300000,thin = 1,burn = 2000,init = c(0,0,0.1),prior.mean1 = 0,prior.var1 = 100,propo.var1 = 0.5,prior.mean0 = 0,prior.var0 = 100,propo.var0 = 0.5,
                    prior.shape = 0.01,prior.scale = 100,propo.varL = 0.1,X = dados2$V1,Y = dados2$V2,sigma = 0.5)
#removing the burning
mcchain<-teste2[[1]][2001:nrow(teste2[[1]]),]
#removing the thinning
mcchain2<-subset(x = mcchain,mcchain$ID%%1==0)#none is removed


#b generating table (need package xtable)
if(DiagnosticFlag){
  tab<-cbind(bmmat(mcmat = teste2[[1]][2001:nrow(teste2[[1]]),-4]),
             t(apply(X = teste2[[1]][2001:nrow(teste2[[1]]),-4],MARGIN = 2,FUN = quantile,probs=c(0.025,0.975))))
  rownames(tab)<-c("Beta0","Beta1","Lambda")
  colnames(tab)<-c("Posteiror mean","MCMC Standard Deviation","2.5%","97.5%")
  xtable(tab,digits = 5)
  
}


#c Correlation between parameters

cor(teste2[[1]][2001:nrow(teste2[[1]]),1],teste2[[1]][2001:nrow(teste2[[1]]),2])


#d Plotting density need package reshape and ggplot2 plyr 
if(DiagnosticFlag){
  plotar<-melt.data.frame(data = mcchain2,id.vars = "ID")
  p<-ggplot(data = plotar)
  p+geom_density(mapping = aes(x = value,group=variable,fill=variable),subset = .(ID > 2000))+
    facet_wrap(~variable,scales = "free")+thema+theme(legend.position="none")
  
}



#e Verifing how much our mcmc is realiable

#generating 25 chains in different initial points
values<-sapply(X = 1:25,simplify = F, FUN = function(x){
  init<-c(rnorm(1,0,5),rnorm(1,0,5),rgamma(1,1,1))
  print(init)
  retornar<-try(data.frame(mh.sampling(N = 30000,thin = 1, burn = 2000,init = init,prior.mean1 = 0,prior.var1 = 100,propo.var1 = 0.1,prior.mean0 = 0,prior.var0 = 100,propo.var0 = 0.1,
                                       prior.shape = 0.01,prior.scale = 100,propo.varL = 0.1,X = dados2$V1,Y = dados2$V2,sigma = 1)[[1]],"MC"=x))
  return(retornar)
  
})
values<-do.call(what = rbind,args = values)
#plotting the chains, expected values and MCMCse by iteractions need reshape ggplot2 GMCM grid gridExtra
if(DiagnosticFlag){
  plotar<-melt.data.frame(data = values,id.vars = c("MC","ID"))
  p<-ggplot(data = plotar)
  p+geom_line(mapping = aes(x = ID,y = value, group = MC, colour = factor(MC)),alpha = 0.2)+thema+
    facet_wrap(facets = ~variable,scales = "free")+theme(legend.position="none")
  tau<-apply(X = matrix(1:3),MARGIN = 1,FUN = function(x){
    GMCM:::cummean(teste2[[1]][,x])
  })
  tau2<-apply(X = matrix(seq(10,nrow(teste2[[1]]),by=1000)),MARGIN = 1,FUN = function(x){
    cbind(bm(vals = teste2[[1]][1:x,1])$se,bm(vals = teste2[[1]][1:x,2])$se,bm(vals = teste2[[1]][1:x,3])$se)
  })
  tau2<-data.frame(t(tau2),"Id"=seq(10,nrow(teste2[[1]]),by=1000))
  names(tau2)<-c("Beta0","Beta1","Lambda","Id")
  plotar<-melt.data.frame(data = tau2,id.vars = "Id")
  p<-ggplot(data = plotar)
  p2<-p+geom_line(mapping = aes(x = Id,y = value,group=variable,color=variable))+thema
  plotar<-data.frame(tau,"Id"=1:nrow(teste2[[1]]))
  names(plotar)<-c("Beta0","Beta1","Lambda","Id")
  plotar<-melt.data.frame(data = plotar,id.vars = "Id")
  p<-ggplot(data = plotar)
  p3<-p+geom_line(mapping = aes(x = Id,y = value,group=variable,colour=variable))+thema
  layout <- matrix(c(1,1,2,3),byrow=T,nrow=2)
  multiplot(plotlist = list(p1,p3,p2),layout = layout)
  
}





#Question 3

#reading data
dados3<-read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")
#Runing mcmc, take something like 8sec
teste3<-mh.sampling(N = 300000,thin = 1,burn = 2000,init = c(0,0,0.1),prior.mean1 = 0,prior.var1 = 100,propo.var1 = 0.5,prior.mean0 = 0,prior.var0 = 100,propo.var0 = 0.5,
                    prior.shape = 0.01,prior.scale = 100,propo.varL = 0.1,X = dados3$V1,Y = dados3$V2,sigma = 0.5)
#removing the burning
mcchain<-teste3[[1]][2001:nrow(teste3[[1]]),]
#removing the thinning
mcchain2<-subset(x = mcchain,mcchain$ID%%1==0)#none is removed


#a generating table (need package xtable)
if(DiagnosticFlag){
  tab<-cbind(bmmat(mcmat = teste3[[1]][2001:nrow(teste3[[1]]),-4]),
             t(apply(X = teste3[[1]][2001:nrow(teste3[[1]]),-4],MARGIN = 2,FUN = quantile,probs=c(0.025,0.975))))
  rownames(tab)<-c("Beta0","Beta1","Lambda")
  colnames(tab)<-c("Posteiror mean","MCMC Standard Deviation","2.5%","97.5%")
  xtable(tab,digits = 5)
  
}




#b Plotting density need package reshape and ggplot2 plyr 
if(DiagnosticFlag){
  plotar<-melt.data.frame(data = mcchain2,id.vars = "ID")
  p<-ggplot(data = plotar)
  p+geom_density(mapping = aes(x = value,group=variable,fill=variable),subset = .(ID > 2000))+
    facet_wrap(~variable,scales = "free")+thema+theme(legend.position="none")
  
}
