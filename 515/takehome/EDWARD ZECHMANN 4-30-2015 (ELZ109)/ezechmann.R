
# Edward ZEchmann
# STAT 515 Take Home Final

# Load the emg.R program to the R environment
source("http://sites.stat.psu.edu/~mharan/515/hwdir/emg.R")

# Load the Batchmenas R-program by Murali Haran to the R-environment
source("http://www.stat.psu.edu/~mharan/batchmeans.R");



# ***********************************************************
# 
# Declare the truncated normal functions 

rtnorm = function( n, a, b, mean, sd ){

    # Initialize the variables
    count=0;
    max_counts=1e6;
    draws=c();
    num_samples=0;

    # Sample from the normal distribution
    while( ( count < max_counts ) & ( num_samples < n) ) {
        count=count+1;
        draws_buffer=rnorm(10*n, mean, sd);
        buf_ix=which( ( draws_buffer > a ) & ( draws_buffer < b ) ) 
        draws=c(draws, draws_buffer[buf_ix]);
        num_samples=length(draws);
    }
    draws=draws[1:n];
    return(draws[1:n]);
}

# buf=rtnorm( n=1, a=0, b=Inf, mean=2, sd=1 );


# ***********************************************************
# 
log_d_tnorm = function (x, a, b, m, sd) {
    p_a=pnorm(q=a, mean = m, sd = sd, lower.tail = TRUE, log.p = FALSE);
    p_b=pnorm(q=b, mean = m, sd = sd, lower.tail = TRUE, log.p = FALSE);
    p_t=p_b-p_a;
    return(dnorm(x, mean = m, sd = sd, log = FALSE)/p_t)
}

# buf2=log_d_tnorm(x=buf, a=0, b=Inf, m=2, sd=1);



# ***********************************************************
# 
# Problem 1 part a
# 

# Change the output directory if necessary
# setwd(C:/Zechmann_Career/Penn_State/STAT_515/2015/Exams/Take_Home_Final)

# read the data set
# use hard coding for final version
xy1 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG1.dat")

# The data set is comes in (x,y) pairs;
# Separate the data into x and y elements 
x1=xy1$V1;
y1=xy1$V2;



emg_B1 = function (x1, y1, max_count, mu_B1, var_B1) {

    # Initialize the constants for the regression 
    lambda1=0.4
    sigma1=1;
    B0=5;


    # Initialize B1_prop to the starting mu_B1 density 
    df_B1=3;
    B1_prop=mu_B1;
    sd_B1=sqrt(var_B1);
    log_d_prop_B1=dt(x=(B1_prop-mu_B1)/sd_B1, df=df_B1, log=TRUE);


    # Use a normal-distribution for the prior distribution 
    # 
    # The prior distribution densities estimate a | Y 
    # 
    prior_mu_B1=0;
    prior_var_B1=100;
    prior_sd_B1=sqrt(prior_var_B1);

    # Calculate the log prior densities of the prior distribution of B1
    #
    log_d_prior_B1=dnorm(x=B1, mean=prior_mu_B1, sd=prior_sd_B1, log=TRUE);


    # Initialize the loglikelihood of the posterior distribution of B1 given Y, X
    # Calculate the likelihood of the posterior distribution of B1 given Y, X
    # This will be in the form of the log likelihood of the regression errors.
    # 


    # Calculate the likelihood of the posterior distribution of B1 given Y, X
    y_hat=B0+B1_prop*x1+1/lambda1;
    error_buf=y1-y_hat;
    log_pd_error=dexpgauss(error_buf, mu = 0, sigma = sigma1, lambda = lambda1, log = TRUE);


    # log_l_pd_star is the log likelihood of the posterior +- a constant 
    log_l_pd_star=sum(log_pd_error)+log_d_prior_B1+log_d_prop_B1;


    # Declare storage variables for each of the data to analyze after the simulation
    B1_a=rep(NA, max_count);
    B1_prop_a=rep(NA, max_count);
    log_p_accept_a=rep(NA, max_count);
    bool_accept_a=rep(NA, max_count);


    # Initilize the varibles which control the loop
    count = 0;

    while( count < max_count ) {

        # increment the count for each iteration
        count=count+1;

        # Draw a random sample from the proposal distribution
        B1_prop=sd_B1*rt(1, df=df_B1)+mu_B1;
        log_d_prop_B1=dt(x=(B1_prop-mu_B1)/sd_B1, df=df_B1, log=TRUE);

        # Save the proposed values for later analysis
        B1_prop_a[count]=B1_prop;

        # Calculate the log prior densities of the prior distribution of B1
        log_d_prior_B1=dnorm(x=B1_prop, mean=prior_mu_B1, sd=prior_sd_B1, log=TRUE);


        # Calculate the likelihood of the posterior distribution of B1 given Y, X
        y_hat=B0+B1_prop*x1+1/lambda1;
        error_buf=y1-y_hat;
        log_pd_error=dexpgauss(error_buf, mu = 0, sigma = sigma1, lambda = lambda1, log = TRUE);

        # log_l_pd is the log likelihood of the posterior of B1 given Y, X +- a constant 
        log_l_pd=sum(log_pd_error)+log_d_prior_B1+log_d_prop_B1;

        # Calcuatae the log probaiblity of acceptance;
        log_p_accept=min(0, log_l_pd - log_l_pd_star);

        # Store acceptance log probability in an array for later analysis     
        log_p_accept_a[count]=log_p_accept;

        bool_accept=( log(runif(1, min=0, max=1) ) < log_p_accept );

        # Store acceptance boolean in an array for later analysis     
        bool_accept_a[count]=bool_accept;

        # Update values for the next iteration based upon teh acceptance boolean
        if( bool_accept == 1 ){
            log_l_pd_star=log_l_pd;
            mu_B1=B1_prop;
            B1_a[count]=B1_prop;
        } else {
            log_l_pd_star=log_l_pd_star;
            B1_a[count]=mu_B1;
        }
    
    }

    B1_a=B1_a[1:count];
    B1_prop_a=B1_prop_a[1:count];
    log_p_accept_a=log_p_accept_a[1:count];
    bool_accept_a=bool_accept_a[1:count];

    return(list(B1=B1_a, B1_prop=B1_prop_a, P_accept=exp(log_p_accept_a), bool_accept=bool_accept_a) );
}


# ***********************************************************
# 
# Problem 1 part a
# Preliminary and final runs


# set the seed
# set.seed(73785);
# output=emg_B1(x1, y1, max_count=1e4, mu_B1=-10, var_B1=1);
# save(output, file="problem1_f10.RData");


# set.seed(73785);
# output=emg_B1(x1, y1, max_count=1e4, mu_B1=0, var_B1=1);
# save(output, file="problem1_f11.RData");


# set the seed
# set.seed(73785);
# output=emg_B1(x1, y1, max_count=1e4, mu_B1=10, var_B1=1);
# save(output, file="problem1_f12.RData");


# set the seed
set.seed(73785);
output=emg_B1(x1, y1, max_count=1e5, mu_B1=3.3, var_B1=1);
# save(output, file="problem1_f13.RData");



# ***********************************************************
# 
# Problem 1 part a
# 

# Part b and c table 
# Posterior means and MCMCse using the batch means program 
# load( file="problem1_f13.RData" );
data_b=matrix(NA, nrow=1, ncol=4);

data_b[1,1]=bm(output$B1)$est;
data_b[1,2]=imse(output$B1);
data_b[1,3:4]=quantile(output$B1, c(0.025, 0.975));


data_b


# ***********************************************************
# 
# Problem 1 Part d table  Plot of the Probability Density

plot(density(output$B1), xlim=c(1, 5), ylim=c(0,2), lty=1, lwd=4, type="l", col="black", main="Posterior Density of the Regression Coeficient", xlab="x-quantile", ylab="Estimated Probability Density", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5);
legend(x=1, y=2, c("B1"), col=c("black"), lty=1, lwd=4, cex=1.5);


# ***********************************************************
# 
# Problem 1 Part e table 
# 
# Make a table of the output of each of the runs
# run_nums=c(10, 11, 12, 13);
# nruns=length(run_nums);
# data_a=matrix(NA, nrow=nruns, ncol=2);

# for (e1 in 1:nruns ){
#     file_name=paste("problem1_f", as.character(run_nums[e1]), ".RData", sep = "");
#     load(file=file_name);

#     n_last=length(output$B1);
#     data_a[e1, 1]=median(output$B1);
#     data_a[e1, 2]=output$B1[n_last];
# 
# }

# data_a





# ***********************************************************
# 
# Problem 2 part a
# 


# read the data set
# use hard coding for final version
# xy2 = scan("C:/Zechmann_Career/Penn_State/STAT_515/2015/Exams/Take_Home_Final/EMG2.dat");
xy2 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG2.dat")


# The data set is comes in (x,y) pairs;
# Separate the data into x and y elements 
x2=xy2$V1;
y2=xy2$V2;


emg_reg = function(x2, y2, max_count, k, mu_B0, mu_B1, mu_lambda, var_B0, var_B1, var_lambda ){


    # Set the minimum number of accepted values
    max_accepted=ceiling(max_count/k);

    # Initialize the constants for the regression 
    sigma2=1;


    # ************************************************************
    # 
    # Draw B0 from a t-distribution with 3 degrees of freedom a mean of mu_B0 and a standard deviation of sd_B0*(df/(df-2).  
    sd_B0=sqrt(var_B0);
    df_B0=3;

    # Draw B0_prop from a t-distribution 
    B0_prop=sd_B0*rt(n=1, df=df_B0)+mu_B0;
    log_d_prop_B0=dt(x=(B0_prop-mu_B0)/sd_B0, df=df_B0, log=TRUE);


    # Use a normal-distribution for the prior distribution 
    # 
    # The prior distribution log density estimate 
    # 
    prior_mu_B0=0;
    prior_var_B0=100;
    prior_sd_B0=sqrt(prior_var_B0);

    # Calculate the log prior densities of the prior distribution of B0
    #
    log_d_prior_B0=dnorm(x=B0_prop, mean=prior_mu_B0, sd=prior_sd_B0, log=TRUE);


    # ************************************************************
    # 
    # Draw B1_prop from a t-distribution with 3 degrees of freedom a mean of mu_B1 and a standard deviation of sd_B1*(df/(df-2).  

    sd_B1=sqrt(var_B1);
    df_B1=3;

    # Draw B1 from a t-distribution 
    B1_prop=sd_B1*rt(n=1, df=df_B1)+mu_B1;
    log_d_prop_B1=dt(x=(B1_prop-mu_B1)/sd_B1, df=df_B1, log=TRUE);


    # Use a normal-distribution for the prior distribution 
    # 
    # The prior distribution log density estimate 
    # 
    prior_mu_B1=0;
    prior_var_B1=100;
    prior_sd_B1=sqrt(prior_var_B1);

    # Calculate the log prior densities of the prior distribution of B1
    #
    log_d_prior_B1=dnorm(x=B1_prop, mean=prior_mu_B1, sd=prior_sd_B1, log=TRUE);


    # ******************************************************
    # 
    # Draw a random sample from the lambda proposal distribution
    sd_lambda=sqrt(var_lambda);

    lambda_prop=rtnorm( n=1, a=0, b=Inf, mean = mu_lambda, sd = sd_lambda );
    log_d_prop_lambda=log_d_tnorm(x=lambda_prop, a=0, b=Inf, m=mu_lambda, sd=sd_lambda);

    # Caculate the density of the prior distribution on lambda
    # 
    rate_lambda=0.01;
    shape_lambda=0.01;

    # The prior for lambda is from a Gamma(0.01, 100) with mean of mu_lambda=1 
    # and a standard deviation of sd_lambada2=10.  
    log_d_prior_lambda=dgamma(x=lambda_prop, shape=shape_lambda, rate = rate_lambda, log = TRUE);


    # Calculate the likelihood of the posterior distribution of B1 given Y, X, B1, lambda, and sigma.
    y_hat=B0_prop+B1_prop*x2+1/lambda_prop;
    error_buf=y2-y_hat;
    log_pd_error=dexpgauss(error_buf, mu = 0, sigma = sigma2, lambda = lambda_prop, log = TRUE);


    # log_l_pd_star is the log likelihood of the posterior plus the log likelihood of the proposal and prior densities
    log_l_pd_star=sum(log_pd_error)+log_d_prior_B0+log_d_prop_B0+log_d_prior_B1+log_d_prop_B1+log_d_prior_lambda+log_d_prop_lambda;

    # Declare storage variables for each of the data to analyze after the simulation
    B0_a=rep(NA, max_accepted);
    B0_prop_a=rep(NA, max_accepted);

    B1_a=rep(NA, max_accepted);
    B1_prop_a=rep(NA, max_accepted);

    lambda_a=rep(NA, max_accepted);
    lambda_prop_a=rep(NA, max_accepted);

    log_p_accept_a=rep(NA, max_accepted);
    bool_accept_a=rep(NA, max_accepted);


    # Initilize the variables which control the loop
    count = 0;
    num_accepted = 0 ;


    while( ( count < max_count ) & (num_accepted < max_accepted ) ) {

        # Increment the count for each iteration
        count=count+1;

        # ******************************************************
        # 
        # Draw a random sample from the B1 proposal distribution
        B0_prop=sd_B0*rt(n=1, df=df_B0)+mu_B0;
        log_d_prop_B0=dt(x=(B0_prop-mu_B0)/sd_B0, df=df_B0, log=TRUE);

        # Calculate the log prior densities of the prior distribution of B0
        #
        log_d_prior_B0=dnorm(x=B0_prop, mean=prior_mu_B0, sd=prior_sd_B0, log=TRUE);

        # ******************************************************
        # 
        # Draw a random sample from the B1 proposal distribution
        B1_prop=sd_B1*rt(n=1, df=df_B1)+mu_B1;
        log_d_prop_B1=dt(x=(B1_prop-mu_B1)/sd_B1, df=df_B1, log=TRUE);

        # Calculate the log prior densities of the prior distribution of B1
        log_d_prior_B1=dnorm(x=B1_prop, mean=prior_mu_B1, sd=prior_sd_B1, log=TRUE);


        # ******************************************************
        # 
        # Draw a random sample from the lambda proposal distribution
        lambda_prop=rtnorm( n=1, a=0, b=Inf, mean = mu_lambda, sd = sd_lambda );
        log_d_prop_lambda=log_d_tnorm(x=lambda_prop, a=0, b=Inf, m=mu_lambda, sd=sd_lambda);


        # ******************************************************
        # 
        #  The prior for lambda 2 is a Gamma(0.01, 100) a mean of mu_lambda=1 and a standard deviation of sd_lambada2=10.  
        # 
        log_d_prior_lambda=dgamma(x=lambda_prop, shape=shape_lambda, rate = rate_lambda, log = TRUE);


        # Calculate the likelihood of the posterior distribution of B1 given Y, X, B1, lambda, and sigma.
        y_hat=B0_prop+B1_prop*x2+1/lambda_prop;
        error_buf=y2-y_hat;
        log_pd_error=dexpgauss(error_buf, mu = 0, sigma = sigma2, lambda = lambda_prop, log = TRUE);

        # log_l_pd is the log likelihood of the posterior +- a constant 
        log_l_pd=sum(log_pd_error)+log_d_prior_B0+log_d_prop_B0+log_d_prior_B1+log_d_prop_B1+log_d_prior_lambda+log_d_prop_lambda;

        # Calculatae the log probaiblity of acceptance;
        log_p_accept=min(0, log_l_pd - log_l_pd_star);

        bool_accept=( log(runif(1, min=0, max=1) ) < log_p_accept );

        # Update values for the next iteration based upon the acceptance boolean
        if( bool_accept == 1 ){

            log_l_pd_star=log_l_pd;
            mu_B0=B0_prop;
            mu_B1=B1_prop;
            mu_lambda=lambda_prop;

            if ( count %% k == 0) { 
                num_accepted=num_accepted+1;
                B0_a[num_accepted]=B0_prop;
                B1_a[num_accepted]=B1_prop;
                lambda_a[num_accepted]=lambda_prop;
            }
        } else {
            if ( count %% k == 0) { 
                num_accepted=num_accepted+1;
                B0_a[num_accepted]=mu_B0;
                B1_a[num_accepted]=mu_B1;
                lambda_a[num_accepted]=mu_lambda;
            }
        }


        # Store the proposed values for further analysis
        if ( count %% k == 0){
            B0_prop_a[num_accepted]=B0_prop;
            B1_prop_a[num_accepted]=B1_prop;
            lambda_prop_a[num_accepted]=lambda_prop;
            log_p_accept_a[num_accepted]=log_p_accept;  
            bool_accept_a[num_accepted]=bool_accept;
        }


    }
    B0_a=B0_a[1:num_accepted];
    B1_a=B1_a[1:num_accepted];
    lambda_a=lambda_a[1:num_accepted];
    B0_prop_a=B0_prop_a[1:num_accepted];
    B1_prop_a=B1_prop_a[1:num_accepted];
    lambda_prop_a=lambda_prop_a[1:num_accepted];
    log_p_accept_a=log_p_accept_a[1:num_accepted];
    bool_accept_a=bool_accept_a[1:num_accepted];

    return(list(B0=B0_a, B1=B1_a, lambda=lambda_a, B0_prop=B0_prop_a, B1_prop=B1_prop_a, lambda_prop=lambda_prop_a, P_accept=exp(log_p_accept_a), bool_accept=bool_accept_a) );
}


# ***********************************************************
# 
# Problem 2 Part a 
# Preliminary, Secondary , and final runs


# Runs of the algorithm

# set the seed
# set.seed(73785);
# output=emg_reg(x2, y2, max_count=1e5, k=10, mu_B0=1, mu_B1=3.5, mu_lambda=0.8, var_B0=0.1, var_B1=0.1, var_lambda=0.1 );
# save(output, file="problem2_f10.RData");


# set the seed
# set.seed(73785);
# output=emg_reg(x2, y2, max_count=1e5, k=10, mu_B0=1, mu_B1=3.5, mu_lambda=0.8, var_B0=0.1, var_B1=0.1, var_lambda=0.01 );
# save(output, file="problem2_f11.RData");


# This is the run which is the best estimate of the data
# set.seed(73785);
output=emg_reg(x2, y2, max_count=1e6, k=10, mu_B0=1, mu_B1=3.5, mu_lambda=0.8, var_B0=0.1, var_B1=0.1, var_lambda=0.01 );
# save(output, file="problem2_f12.RData");


# set the seed
# set.seed(73785);
# output=emg_reg(x2, y2, max_count=1e4, k=10, mu_B0=10, mu_B1=3.5, mu_lambda=10, var_B0=2, var_B1=2, var_lambda=2 );
# save(output, file="problem2_f20.RData");


# set the seed
# set.seed(73785);
# output=emg_reg(x2, y2, max_count=1e4, k=10, mu_B0=0.1, mu_B1=3.5, mu_lambda=0.1, var_B0=2, var_B1=2, var_lambda=2 );
# save(output, file="problem2_f30.RData");


# set the seed
# set.seed(73785);
# output=emg_reg(x2, y2, max_count=1e4, k=10, mu_B0=10, mu_B1=0, mu_lambda=10, var_B0=2, var_B1=2, var_lambda=2 );
# save(output, file="problem2_f40.RData");


# set the seed
# set.seed(73785);
# output=emg_reg(x2, y2, max_count=1e4, k=10, mu_B0=0.5, mu_B1=2, mu_lambda=0.3, var_B0=2, var_B1=2, var_lambda=2 );
# save(output, file="problem2_f50.RData");


# set the seed
# set.seed(73785);
# output=emg_reg(x2, y2, max_count=1e4, k=10, mu_B0=0, mu_B1=0, mu_lambda=0.001, var_B0=2, var_B1=2, var_lambda=2 );
# save(output, file="problem2_f60.RData");


# set the seed
# set.seed(73785);
# output=emg_reg(x2, y2, max_count=1e5, k=10, mu_B0=0, mu_B1=0, mu_lambda=0.001, var_B0=0.1, var_B1=0.1, var_lambda=0.01 );
# save(output, file="problem2_f61.RData");


# set the seed
# set.seed(73785);
# output=emg_reg(x2, y2, max_count=1e4, k=10, mu_B0=0, mu_B1=-10, mu_lambda=0.001, var_B0=2, var_B1=2, var_lambda=2 );
# save(output, file="problem2_f70.RData");


# set the seed
# set.seed(73785);
# output=emg_reg(x2, y2, max_count=1e4, k=10, mu_B0=0, mu_B1=-10, mu_lambda=10, var_B0=2, var_B1=2, var_lambda=2 );
# save(output, file="problem2_f80.RData");


# set the seed
# set.seed(73785);
# output=emg_reg(x2, y2, max_count=1e5, k=10, mu_B0=0, mu_B1=-10, mu_lambda=10, var_B0=0.1, var_B1=0.1, var_lambda=0.01 );
# save(output, file="problem2_f81.RData");


# ***********************************************************
# 
# Problem 2 Part b 
# 
# 
# Posterior means and MCMCse using the batch means program 
# load( file="problem2_f12.RData" );
data_b=matrix(NA, nrow=3, ncol=4);
data_b[1,1]=bm(output$B0)$est;
data_b[1,2]=imse(output$B0);
data_b[1,3:4]=quantile(output$B0, c(0.025, 0.975));

data_b[2,1]=bm(output$B1)$est;
data_b[2,2]=imse(output$B1);
data_b[2,3:4]=quantile(output$B1, c(0.025, 0.975));

data_b[3,1]=bm(output$lambda)$est;
data_b[3,2]=imse(output$lambda);
data_b[3,3:4]=quantile(output$lambda, c(0.025, 0.975));

data_b

# ***********************************************************
# 
# Problem 2 Part c table Correlation fo B0 and B1
# 
cor_data1=cor(x=output$B0, y=output$B1, use = "everything", method = "pearson");
cor_data1
cor_data2=cor.test(x=output$B0, y=output$B1, alternative = "two.sided", method = "pearson", exact = NULL, conf.level = 0.95);
cor_data2$conf.int



# ***********************************************************
# 
# Problem 2 Part d table  Plot of the Probability Densities 

plot(density(output$B0), xlim=c(0, 5), ylim=c(0,8), lty=1, lwd=4, type="l", col="black", main="Posterior Density of the Regression Coeficients", xlab="x-quantile", ylab="Estimated Probability Density", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5);
lines(density(output$B1), lty=2, lwd=4, type="l", col="red");
lines(density(output$lambda), lty=3, lwd=4, type="l", col="green");
legend(x=2, y=8, c("B0", "B1", "lambda"), col=c("black", "red", "green"), lty=3, lwd=4, cex=1.5);


# Part e table 
# Make a table of the output of each of the runs
# run_nums=c(10, 11, 12, 20, 30, 40, 50, 60, 61, 70, 80, 81);
# nruns=length(run_nums);
# data_a=matrix(NA, nrow=nruns, ncol=6);

# for (e1 in 1:nruns ){
#     file_name=paste("problem2_f", as.character(run_nums[e1]), ".RData", sep = "");
#     load(file=file_name);

#     n_last=length(output$B0);

#     data_a[e1, 1]=data_a[e1, 1]=median(output$B0);
#     data_a[e1, 2]=median(output$B1);
#     data_a[e1, 3]=median(output$lambda);

#     data_a[e1, 4]=output$B0[n_last];
#     data_a[e1, 5]=output$B1[n_last];
#     data_a[e1, 6]=output$lambda[n_last];

# }

# data_a


# ***********************************************************
# 
# Problem 3  Part a


# read the data set
# use the online data set
xy3 = read.table("http://sites.stat.psu.edu/~mharan/515/hwdir/EMG3.dat")


# The data set is comes in (x,y) pairs;
# Separate the data into x and y elements 
x3=xy3$V1;
y3=xy3$V2;


# ***********************************************************
# 
# Problem 3  Part a
# 
# Preliminary and Final Runs of the algorithm 

# set the seed
# set.seed(73785);
# output=emg_reg(x3, y3, max_count=1e4, k=10, mu_B0=10, mu_B1=10, mu_lambda=10, var_B0=2, var_B1=2, var_lambda=2 );
# save(output, file="problem3_f10.RData");


# set the seed
# set.seed(73785);
# output=emg_reg(x3, y3, max_count=1e4, k=10, mu_B0=-10, mu_B1=0, mu_lambda=0.001, var_B0=2, var_B1=2, var_lambda=2 );
# save(output, file="problem3_f11.RData");


# set the seed
# set.seed(73785);
# output=emg_reg(x3, y3, max_count=1e4, k=10, mu_B0=10, mu_B1=-10, mu_lambda=0.001, var_B0=0.1, var_B1=0.1, var_lambda=0.1 );
# save(output, file="problem3_f12.RData");


# set the seed
# set.seed(73785);
# output=emg_reg(x3, y3, max_count=1e4, k=10, mu_B0=0, mu_B1=-10, mu_lambda=0.001, var_B0=0.1, var_B1=0.1, var_lambda=0.1 );
 #save(output, file="problem3_f13.RData");

# set the seed
# set.seed(785);
# output=emg_reg(x3, y3, max_count=1e4, k=10, mu_B0=-6, mu_B1=2.5, mu_lambda=0.16, var_B0=0.1, var_B1=0.1, var_lambda=0.1 );
 #save(output, file="problem3_f14.RData");


# set the seed
# set.seed(785);
# output=emg_reg(x3, y3, max_count=1e5, k=50, mu_B0=-6, mu_B1=2.5, mu_lambda=0.16, var_B0=0.1, var_B1=0.1, var_lambda=0.001 );
# save(output, file="problem3_f15.RData");


# Final Run
# 
# set the seed
# 
set.seed(785);
output=emg_reg(x3, y3, max_count=1e6, k=50, mu_B0=-6, mu_B1=2.5, mu_lambda=0.16, var_B0=0.1, var_B1=0.1, var_lambda=0.01 );
save(output, file="problem3_f16.RData");




# ***********************************************************
# 
# Problem 3  Part b table 
# 
# 
# Posterior means and MCMCse using the batch means program 
load( file="problem3_f16.RData" );
data_b=matrix(NA, nrow=3, ncol=4);
data_b[1,1]=bm(output$B0)$est;
data_b[1,2]=imse(output$B0);
data_b[1,3:4]=quantile(output$B0, c(0.025, 0.975));

data_b[2,1]=bm(output$B1)$est;
data_b[2,2]=imse(output$B1);
data_b[2,3:4]=quantile(output$B1, c(0.025, 0.975));

data_b[3,1]=bm(output$lambda)$est;
data_b[3,2]=imse(output$lambda);
data_b[3,3:4]=quantile(output$lambda, c(0.025, 0.975));

data_b


# ***********************************************************
# 
# Problem 3  Part c table Correlation fo B0 and B1
# 
# 
cor_data1=cor(x=output$B0, y=output$B1, use = "everything", method = "pearson");
cor_data1
cor_data2=cor.test(x=output$B0, y=output$B1, alternative = "two.sided", method = "pearson", exact = NULL, conf.level = 0.95);
cor_data2$conf.int


# ***********************************************************
# 
# Problem 3   Part d table  Plot of the Probability Densities 

plot(density(output$B0), xlim=c(-8, 10), ylim=c(0,40), lty=1, lwd=4, type="l", col="black", main="Posterior Density of the Regression Coeficients", xlab="x-quantile", ylab="Estimated Probability Density", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5);
lines(density(output$B1), lty=2, lwd=4, type="l", col="red");
lines(density(output$lambda), lty=3, lwd=4, type="l", col="green");
legend(x=2, y=20, c("B0", "B1", "lambda"), col=c("black", "red", "green"), lty=3, lwd=4, cex=1.5);


# ***********************************************************
# 
# Problem 3  Part e table 
# 
# 
# Make a table of the output of each of the runs
#run_nums=c(10, 11, 12, 13, 14, 15, 16);
#nruns=length(run_nums);
#data_a=matrix(NA, nrow=nruns, ncol=6);

#for (e1 in 1:nruns ){
#    file_name=paste("problem3_f", as.character(run_nums[e1]), ".RData", sep = "");
#    load(file=file_name);

#    n_last=length(output$B0);

#    data_a[e1, 1]=data_a[e1, 1]=median(output$B0);
#    data_a[e1, 2]=median(output$B1);
#    data_a[e1, 3]=median(output$lambda);

#    data_a[e1, 4]=output$B0[n_last];
#    data_a[e1, 5]=output$B1[n_last];
#    data_a[e1, 6]=output$lambda[n_last];

#}

#data_a





