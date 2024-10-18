% ## Matlab version of Murali Haran's consistent batch means and imse estimators of Monte Carlo standard errors
% ## author: Weina Ge, Department of Computer Science, Penn State University
% 
% ## An R function for computing consistent batch means estimate of standard error from: 
% ## Citation: Galin L. Jones, Murali Haran, Brian S. Caffo, and Ronald Neath, "Fixed-Width Output Analysis for Markov Chain Monte Carlo" (2006), Journal of the American Statistical Association, 101:1537--1547
% 
% ## input: vals, a vector of N values (from a Markov chain),bs=batch size
% ## default bs (batch size) is "sqroot"=> number of batches is the square root of the run length
% ## if bs is "cuberoot", number of batches is the cube root of the run length
% ## output: list consisting of estimate of expected value and the Monte Carlo standard error of the estimate
% 
% ## NOTE: YOU DO NOT NEED TO DOWNLOAD THIS FILE TO RUN BATCHMEANS IN R
% ## SIMPLY USE THE COMMAND BELOW FROM YOUR R COMMAND LINE
% ## source("http://www.stat.psu.edu/~mharan/batchmeans.R")
% 
% ## Input: vals is a matrix of values from a Markov chain produced by the
% Metropolis-Hastings algorithm; each row is a vector of samples for the
% corresponding parameter
% ## bs provides the batch size, either "sqroot" for the square root of the sample size (recommended)
% ## or "cuberoot" for cube root of the sample size
function [est, se]=bm(vals,bs,warn)

if nargin < 2
    bs = 'sqroot'
end

if nargin < 3
    warn = false
end

N = size(vals,2)
if (N<1000)
    if warn
        disp('WARNING: too few samples (less than 1000)')
    end
    return
end

switch lower(bs)
    case 'sqroot'
        b = floor(sqrt(N)); %batch size
        a = floor(N/b); % number of batches
    case 'cuberoot'
        b = floor(N^(1/3)); %batch size
        a = floor(N/b); % number of batches
    otherwise %batch size provided
        if isnumeric(bs) == 0
            return
        end
        b = floor(bs) % batch size
        if (b > 1) % batch size valid
          a = floor(N/b) % number of batches
        else
          disp('batch size invalid')
          return
        end        
end
Ys = zeros(size(vals,1),a);
for k=1:a
Ys(:,k) = mean(vals(:,((k-1)*b+1):(k*b)),2);
end

muhat = mean(Ys,2);
sigmahatsq = b*sum((Ys-repmat(muhat,1,a)).^2,2)/(a-1);
bmse = sqrt(sigmahatsq/N);

est = muhat;
se = bmse;

return
