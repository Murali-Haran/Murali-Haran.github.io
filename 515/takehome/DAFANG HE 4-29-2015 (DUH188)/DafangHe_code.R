##code  dafanghe####
##stat 515 take home exam ###
##problem 1###
## (a) ###
source('http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R')
source('http://sites.stat.psu.edu/~mharan/batchmeans.R')
dat1=read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat')
##algorithm description:
#Basically here we need to calcualte P(B|Y,X) ~ P(B) * P(Y|X,B)
#P(B) = Norm
#P(Y|X,B) could be the emg
#so we do motriopoli sampling on this
B0=5
lamda=0.4
sigma=1
X <- dat1[1:100,1]
Y <- dat1[1:100,2]
#the prior for B1
prior <- function(x) {
  log(dnorm(x,0,10))
}
#the likelihood
Likelihood <- function(B1)
{
  pro = 0
  for( index in 1:100 )
  {
    cur_mu=B0 + B1 * X[index]
    cur_pro = dexpgauss(Y[index],cur_mu,sigma,lamda, TRUE)
    pro = pro + cur_pro
  }
  return(pro)
}
#posterior distribution
posterior <- function(x){
  return (prior(x) + Likelihood(x) )
}

#first plot the distribution of the posterior to see what it's looks like
testing = seq(4, 10, 0.05)
len=length(testing)
result = rep(NA, len)
for(index in 1:len)
{
   cur_value = testing[index]
   p = exp(posterior(cur_value) )
   result[index]=p
}
plot(testing,result)

## according to the plot,chose variance as 1
## the proposal function
proposalfunction <- function(x, param)
{
  return( rnorm(1, x, param))
}
metropolis_MCMC <- function(init_value, iteration, param)
{
  chain = array(dim = c(iteration + 1,1))
  chain[1]=init_value
  for( i in 1:iteration )
  {
    proposal = proposalfunction(chain[i],param)
    probab =exp(posterior(proposal) - posterior(chain[i]) )
    rand=runif(1)
    if( rand < probab)
    {
      chain[i+1]=proposal
    }
    else{
      chain[i+1] = chain[i]
    }
  }
  return(chain)
}
##init value distribution
#besed on the observation of posterior distribution plot, we can estimate roughly the mean
#is about 7.+, so init here
generate_init <- function()
{
  return(rnorm(1,7.5,1))
}
#random init value from (0-1)
init_value=generate_init()
number_of_iteration=30000
chain = metropolis_MCMC(init_value, number_of_iteration,1)

###(b) ###
##result:
bm(chain)
#$est
#[1] 7.336929

#$se
#[1] 0.01161353

###(c) ###
###95% interval ##
quantile(chain,c(0.025, 0.975))
##result:
#    2.5%    97.5% 
#6.697084 7.922604  

##(d) ###
plot(density(chain))

##(e) ###
# standard error is small and when I increase the number of iteration to 50000
#random init value from (0-1)
init_value=runif(1)
number_of_iteration=50000
chain = metropolis_MCMC(init_value, number_of_iteration,1)
#the mean and standard error:
#$est
#[1] 7.338416
#$se
#[1] 0.003657136
#we could see that the as the number of iteration increase, the standard error has been smaller.
#but the mean remain similar. It means the previous result is trustworthing. We could increase 
#the iteration to lower the standard error


## problem 2 ###
dat2=read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat')
X <- dat2[,1]
Y <- dat2[,2]
##prior
prior_B0 <-function(x)
{
  return(log(dnorm(x,0,10) ) )
}
prior_B1 <-function(x)
{
  return(log(dnorm(x,0,10)) )
}
prior_lamda <- function(x)
{
  pro_value = dgamma(x,shape=0.01,scale=100)
  return(log(pro_value))
}
##likelihood
likelihood <- function(B0,B1,lamda)
{
  pro = 0
  for( index in 1:100 )
  {
    cur_mu=B0 + B1 * X[index]
    cur_pro = dexpgauss(Y[index],cur_mu,sigma,lamda, TRUE)
    pro = pro + cur_pro
  }
  return(pro)
}
##posterior 
posterior_B0 <- function(B0,B1,lamda)
{
  return(prior_B0(B0) +  likelihood(B0,B1,lamda) )
}

posterior_B1 <- function(B0,B1,lamda)
{
  return(prior_B1(B1) + likelihood(B0,B1,lamda) )
}

posterior_lamda <- function(B0,B1,lamda)
{
  prior_pro = prior_lamda(lamda)
  if(prior_pro == -Inf)
  {
    return(-Inf)
  }
  return(prior_lamda(lamda) + likelihood(B0,B1,lamda) )
}

#proposal function
proposalfunction <- function(x, param)
{
  return( rnorm(1, x, param))
}

proposalfunciton_lamda <- function(x)
{
  return( rnorm(1, x, param))
}
##Fix B0 and lamda as before, we know that the posterior of B1 is similar to normal
##fix B1 and lamda as B1=7, lamda=0.4 we plot the posterior of B0
B1=7
lamda=0.4
values=seq(-10,20,0.1)
len=length(values)
result = rep(NA,len)
for(i in 1:len)
{
  cur_value = values[i]
  result[i] = exp( posterior_B0(cur_value, B1, lamda) )
}
plot(values, result)

##fix B0=5, B1=7, we plot the posterior of lamda
B1=7
B0=5
values=seq(1,20,0.1)
len=length(values)
result = rep(NA,len)
for(i in 1:len)
{
  cur_value = values[i]
  result[i] = exp( posterior_lamda(B0, B1, cur_value) )
}
plot(values, result)
## (a) ##
metropolis_MCMC <- function(init_value_B0, init_value_B1, init_value_lamda, iteration)
{
  chain_B0=array(dim = c(iteration+1,1))
  chain_B1 = array(dim = c(iteration + 1,1))
  chain_lamda = array(dim = c(iteration + 1,1))
  chain_B0[1]=init_value_B0
  chain_B1[1]=init_value_B1
  chain_lamda[1]=init_value_lamda
  param=1
  for( i in 1:iteration )
  {
    #first update B0
    if(i%%1000==0)
    {
      print("finish 500")
    }
    proposal = proposalfunction(chain_B0[i], param)
    probab = exp(posterior_B0(proposal, chain_B1[i],chain_lamda[i]) - posterior_B0(chain_B0[i],chain_B1[i],chain_lamda[i]) )
    rand=runif(1)
    if( rand < probab)
    {
      chain_B0[i+1]=proposal
    }
    else{
      chain_B0[i+1] = chain_B0[i]
    }
    #print("update chain_B0:")
    #print(chain_B0[i+1])
    #then update B1 based on current B0 and lamda
    proposal = proposalfunction(chain_B1[i], param)
    probab = exp(posterior_B1(chain_B0[i+1],proposal,chain_lamda[i]) - posterior_B1(chain_B0[i+1],chain_B1[i],chain_lamda[i]) )
    rand=runif(1)
    if( rand < probab)
    {
      chain_B1[i+1] = proposal
    }
    else{
      chain_B1[i+1] = chain_B1[i]
    }
    #print("update chain_B1:")
    #print(chain_B1[i+1])
    #then update lamda based on current B1 and lamda
    proposal = proposalfunction(chain_lamda[i], 0.1 * param)
    probab = exp(posterior_lamda(chain_B0[i+1], chain_B1[i+1],proposal) - posterior_lamda(chain_B0[i+1],chain_B1[i+1],chain_lamda[i]) )
    rand=runif(1)
    if( rand < probab)
    {
      chain_lamda[i+1] = proposal
    }
    else{
      chain_lamda[i+1] = chain_lamda[i]
    }
    #print("update chain_lamda:")
    #print(chain_lamda[i+1])
  }
  result_chain=cbind(chain_B0,chain_B1,chain_lamda)
  return(result_chain)
}


#randonly init value for 3 value
#random init value from (0-1)
init_value_B0=runif(1)
init_value_B1=runif(1)
init_value_lamda=runif(1)
number_of_iteration=50000
chain = metropolis_MCMC(init_value_B0,init_value_B1, init_value_lamda, number_of_iteration)
chain_B0=chain[,1]
chain_B1=chain[,2]
chain_lamda=chain[,3]

##(a) ##
##formula
##(1) Using mh algorithm to sample B0
##(2) Using mh algorithm to sample B1
##(3) Using mh algorithm to sample lamda

##(b) ##
##mean and standard eroor
bm(chain_B0)
bm(chain_B1)
bm(chain_lamda)
#95% credible intervals
quantile(chain_B0,c(0.025,0.975))
quantile(chain_B1,c(0.025,0.975))
quantile(chain_lamda,c(0.025,0.975))


##(c) ##
##correlation between B0 and B1
cor(chain_B0,chain_B1)

##(d) ##
##density plot###
plot(density(chain_B0))
plot(density(chain_B1))
plot(density(chain_lamda))

##(e) ##
acf(chain_B0)
acf(chain_B1)
acf(chain_lamda)

##problem 3
dat3=read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat')
X <- dat3[1:100,1]
Y <- dat3[1:100,2]
init_value_B0=runif(1)
init_value_B1=runif(1)
init_value_lamda=runif(1)
number_of_iteration=150000
chain = metropolis_MCMC(init_value_B0,init_value_B1, init_value_lamda, number_of_iteration)
chain_B0=chain[,1]
chain_B1=chain[,2]
chain_lamda=chain[,3]

##(a)
##mean and standard eroor
bm(chain_B0)
bm(chain_B1)
bm(chain_lamda)
#95% credible intervals
quantile(chain_B0,c(0.025,0.975))
quantile(chain_B1,c(0.025,0.975))
quantile(chain_lamda,c(0.025,0.975))

##(b)
##density plot###
plot(density(chain_B0))
plot(density(chain_B1))
plot(density(chain_lamda))

##(c)
acf(chain_B0)
acf(chain_B1)
acf(chain_lamda)
