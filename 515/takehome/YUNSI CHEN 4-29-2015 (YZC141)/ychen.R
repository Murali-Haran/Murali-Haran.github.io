#final

#Question 1

source('http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R');
source('http://sites.stat.psu.edu/~mharan/batchmeans.R');
data1 = read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat', col.names = c('X','Y'));

#part (a)

set.seed(1);

#define objective posterior distribution function h
h = function(beta1) 
{
  prod(22*dexpgauss(data1$Y, mu = beta0 + beta1*data1$X, sigma, lambda, log = FALSE))*dnorm(beta1, 0, 100, log = FALSE);
}

#set up simulation factors
A = list();
n = seq(1000, 10000, by = 200);
tau2 = 1;
beta0 = 5;
lambda = 0.4;
sigma = 1;

#apply A-M-H sampling to generate sample size of posterior dist from 1000 to 10000
for (j in 1:length(n))
{  
  beta1 = c();
  i = 1;
  beta1[i] = 0;
  while (i < n[j])
  {
    U = runif(1, 0, 1);
    z = rnorm(1, beta1[i], tau2);
    if (h(beta1[i])*dnorm(beta1[i], z, tau2) > 0)
    {
      p = min(1, (h(z)*dnorm(z, beta1[i], tau2))/(h(beta1[i])*dnorm(beta1[i], z, tau2)));
    }  
    else if (h(beta1[i])*dnorm(beta1[i], z, tau2) == 0)
    {
      p = 1;
    }
    i = i + 1;
    if (U < p)
    {
      beta1[i] = z;
    }
    else
    {
      beta1[i] = beta1[i - 1];
    }
  }  
  A[[j]] = beta1; 
}

#part (e)

#check sample efficiency

#mu and mcse
mean = c();
mcse = c();
for (k in 1:length(n))
{
  mean[k] = bm(A[[k]], bs="sqroot", warn = TRUE)$est;
  mcse[k] = bm(A[[k]], bs="sqroot", warn = TRUE)$se;
} 
plot(n, mean, main = '');
lines(n, mean);
plot(n, mcse, main = '');
lines(n, mcse);

#sample standard error (sse)
v = c();
for (k in 1:length(n))
{
  v[k] = imse(A[[k]], asymvar = TRUE);
} 
plot(n, v, main = '');
lines(n, v);

#acf
acf(A[[46]], main = '');

#part (b)

#calculate mu and mcse
mean[46];
mcse[46];

#part (c)

#calculate the 95% confidence interval
quantile(A[[46]], c(0.025, 0.975));

#part (d)

#plot density with the full size and half of the size
plot(density(A[[46]]), main = '', axes = FALSE, xlab = '', ylab = '');
par(new = TRUE);
plot(density(A[[46]][1:5000]), main = '', col = 2, lty = 2);
legend(0, 1.2, c('density.n', 'density.n/2'), col = c(1, 2), lty = c(1, 2));

#Question 2

source('http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R');
source('http://sites.stat.psu.edu/~mharan/batchmeans.R');
data2 = read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat', col.names = c('X','Y'));

#part (a)

set.seed(1);

#define objective posterior distribution function h
h = function(beta0, beta1, lambda) 
{
  prod(16*dexpgauss(data2$Y, mu = beta0 + beta1*data2$X, sigma, lambda, log = FALSE))*dnorm(beta0, 0, 100, log = FALSE)*dnorm(beta1, 0, 100, log = FALSE)*dgamma(lambda, 0.01, rate = 100, log = FALSE);
}

#set up simulation factors
B = list();
n = seq(1000, 10000, by = 200);
tau21 = 0.1;
tau22 = 0.1;
tau23 = 0.1;
sigma = 1;

#apply A-M-H sampling to generate sample size of posterior dist from 1000 to 10000
for (j in 1:length(n))
{  
  beta0 = c();
  beta1 = c();
  lambda = c();
  i = 1;
  beta0[i] = 0;
  beta1[i] = 0;
  lambda[i] = 0.01;
  while (i < n[j])
  {
    U = runif(1, 0, 1);
    z1 = rnorm(1, beta0[i], tau21);
    z2 = rnorm(1, beta1[i], tau22);
    z3 = 0;
    while (z3 <= 0)
    {
      z3 = rnorm(1, lambda[i], tau23);
    }
    if (h(beta0[i],beta1[i],lambda[i])*dnorm(beta0[i], z1, tau21)
        *dnorm(beta1[i], z2, tau22)*dnorm(lambda[i], z3, tau23) > 0)
    {
      p = min(1, h(z1, z2, z3)/h(beta0[i],beta1[i],lambda[i]));
    }  
    else if (h(beta0[i],beta1[i],lambda[i])*dnorm(beta0[i], z1, tau21)
             *dnorm(beta1[i], z2, tau22)*dnorm(lambda[i], z3, tau23) == 0)
    {
      p = 1;
    }
    i = i + 1;
    if (U < p)
    {
      beta0[i] = z1;
      beta1[i] = z2;
      lambda[i] = z3;
    }
    else
    {
      beta0[i] = beta0[i - 1];
      beta1[i] = beta1[i - 1];
      lambda[i] = lambda[i - 1];
    }
  }  
  B[[j]] = cbind(beta0, beta1, lambda); 
}

#part (e)

#check sample efficiency

#mu and mcse
mean = matrix(NA, length(n), 3);
mcse = matrix(NA, length(n), 3);
for (k in 1:length(n))
{
  mean[k, ] = as.data.frame(bmmat(B[[k]], skip = NA))$est;
  mcse[k, ] = as.data.frame(bmmat(B[[k]], skip = NA))$se;
} 
plot(n, mean[, 1], main = '');
lines(n, mean[, 1]);
plot(n, mean[, 2], main = '');
lines(n, mean[, 2]);
plot(n, mean[, 3], main = '');
lines(n, mean[, 3])
plot(n, mcse[, 1], main = '');
lines(n, mcse[, 1]);
plot(n, mcse[, 2], main = '');
lines(n, mcse[, 2]);
plot(n, mcse[, 3], main = '');
lines(n, mcse[, 3])

#sample standard error (sse)
v = matrix(NA, length(n), 3);;
for (k in 1:length(n))
{
  v[k, 1] = imse(B[[k]][, 1], asymvar = TRUE);
  v[k, 2] = imse(B[[k]][, 2], asymvar = TRUE);
  v[k, 3] = imse(B[[k]][, 3], asymvar = TRUE);
} 
plot(n, v[, 1], main = '');
lines(n, v[, 1]);
plot(n, v[, 2], main = '');
lines(n, v[, 2]);
plot(n, v[, 3], main = '');
lines(n, v[, 3]);

#acf
acf(B[[46]][, 1], main = '');
acf(B[[46]][, 2], main = '');
acf(B[[46]][, 3], main = '');

#part (b)

#calculate mu and mcse
mean[46,];
mcse[46,];

#calculate the 95% confidence interval
quantile(B[[46]][, 1], c(0.025, 0.975));
quantile(B[[46]][, 2], c(0.025, 0.975));
quantile(B[[46]][, 3], c(0.025, 0.975));

#part (c)

#correlation between beta0 and beta1 in posterior
cor(B[[46]][, 1], B[[46]][, 2]);

#part (d)

#plot density with the full size and half of the size
plot(density(B[[46]][, 1]), main = '', axes = FALSE, xlab = '', ylab = '');
par(new = TRUE);
plot(density(B[[46]][1:5000, 1]), main = '', col = 2, lty = 2);
legend(0, 2.5, c('density.n', 'density.n/2'), col = c(1, 2), lty = c(1, 2));
plot(density(B[[46]][, 2]), main = '', axes = FALSE, xlab = '', ylab = '');
par(new = TRUE);
plot(density(B[[46]][1:5000, 2]), main = '', col = 2, lty = 2);
legend(0, 2, c('density.n', 'density.n/2'), col = c(1, 2), lty = c(1, 2));
plot(density(B[[46]][, 1]), main = '', axes = FALSE, xlab = '', ylab = '');
par(new = TRUE);
plot(density(B[[46]][1:5000, 3]), main = '', col = 2, lty = 2);
legend(0, 10, c('density.n', 'density.n/2'), col = c(1, 2), lty = c(1, 2));

#Qestion 3

source('http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R');
source('http://sites.stat.psu.edu/~mharan/batchmeans.R');
data3 = read.table('http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat', col.names = c('X','Y'));

#main

set.seed(1);

#define objective posterior distribution function h
h = function(beta0, beta1, lambda) 
{
  prod(22*dexpgauss(data3$Y, mu = beta0 + beta1*data3$X, sigma, lambda, log = FALSE))*dnorm(beta0, 0, 100, log = FALSE)*dnorm(beta1, 0, 100, log = FALSE)*dgamma(lambda, 0.01, rate = 100, log = FALSE);
}

#set up simulation factors
C = list();
n = seq(1000, 10000, by = 200);
tau21 = 0.1;
tau22 = 0.1;
tau23 = 0.1;
sigma = 1;

#apply A-M-H sampling to generate sample size of posterior dist from 1000 to 10000
for (j in 1:length(n))
{  
  beta0 = c();
  beta1 = c();
  lambda = c();
  i = 1;
  beta0[i] = 0;
  beta1[i] = 0;
  lambda[i] = 0.01;
  while (i < n[j])
  {
    U = runif(1, 0, 1);
    z1 = rnorm(1, beta0[i], tau21);
    z2 = rnorm(1, beta1[i], tau22);
    z3 = 0;
    while (z3 <= 0)
    {
      z3 = rnorm(1, lambda[i], tau23);
    }
    if (h(beta0[i],beta1[i],lambda[i])*dnorm(beta0[i], z1, tau21)
        *dnorm(beta1[i], z2, tau22)*dnorm(lambda[i], z3, tau23) > 0)
    {
      p = min(1, h(z1, z2, z3)/h(beta0[i],beta1[i],lambda[i]));
    }  
    else if (h(beta0[i],beta1[i],lambda[i])*dnorm(beta0[i], z1, tau21)
             *dnorm(beta1[i], z2, tau22)*dnorm(lambda[i], z3, tau23) == 0)
    {
      p = 1;
    }
    i = i + 1;
    if (U < p)
    {
      beta0[i] = z1;
      beta1[i] = z2;
      lambda[i] = z3;
    }
    else
    {
      beta0[i] = beta0[i - 1];
      beta1[i] = beta1[i - 1];
      lambda[i] = lambda[i - 1];
    }
  }  
  C[[j]] = cbind(beta0, beta1, lambda); 
}

#part (c)

#check sample efficiency

#mu and mcse
mean = matrix(NA, length(n), 3);
mcse = matrix(NA, length(n), 3);
for (k in 1:length(n))
{
  mean[k, ] = as.data.frame(bmmat(C[[k]], skip = NA))$est;
  mcse[k, ] = as.data.frame(bmmat(C[[k]], skip = NA))$se;
} 
plot(n, mean[, 1], main = '');
lines(n, mean[, 1]);
plot(n, mean[, 2], main = '');
lines(n, mean[, 2]);
plot(n, mean[, 3], main = '');
lines(n, mean[, 3])
plot(n, mcse[, 1], main = '');
lines(n, mcse[, 1]);
plot(n, mcse[, 2], main = '');
lines(n, mcse[, 2]);
plot(n, mcse[, 3], main = '');
lines(n, mcse[, 3])

#sample standard error (sse)
v = matrix(NA, length(n), 3);;
for (k in 1:length(n))
{
  v[k, 1] = imse(C[[k]][, 1], asymvar = TRUE);
  v[k, 2] = imse(C[[k]][, 2], asymvar = TRUE);
  v[k, 3] = imse(C[[k]][, 3], asymvar = TRUE);
} 
plot(n, v[, 1], main = '');
lines(n, v[, 1]);
plot(n, v[, 2], main = '');
lines(n, v[, 2]);
plot(n, v[, 3], main = '');
lines(n, v[, 3]);

#acf
acf(C[[46]][, 1], main = '');
acf(C[[46]][, 2], main = '');
acf(C[[46]][, 3], main = '');

#part (a)

#calculate mu and mcse
mean[46,];
mcse[46,];

#calculate the 95% confidence interval
quantile(C[[46]][, 1], c(0.025, 0.975));
quantile(C[[46]][, 2], c(0.025, 0.975));
quantile(C[[46]][, 3], c(0.025, 0.975));

#part (b)

#plot density with the full size and half of the size
plot(density(C[[46]][, 1]), main = '', axes = FALSE, xlab = '', ylab = '');
par(new = TRUE);
plot(density(C[[46]][1:5000, 1]), main = '', col = 2, lty = 2);
legend(0.8, 2.5, c('density.n', 'density.n/2'), col = c(1, 2), lty = c(1, 2));
plot(density(C[[46]][, 2]), main = '', axes = FALSE, xlab = '', ylab = '');
par(new = TRUE);
plot(density(C[[46]][1:5000, 2]), main = '', col = 2, lty = 2);
legend(0, 2, c('density.n', 'density.n/2'), col = c(1, 2), lty = c(1, 2));
plot(density(C[[46]][, 1]), main = '', axes = FALSE, xlab = '', ylab = '');
par(new = TRUE);
plot(density(C[[46]][1:5000, 3]), main = '', col = 2, lty = 2);
legend(0.1, 60, c('density.n', 'density.n/2'), col = c(1, 2), lty = c(1, 2));