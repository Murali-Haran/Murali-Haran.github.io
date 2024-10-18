//To compile: gcc -o imse imse.c -lm 
/* compute imse estimate of variance/standard error */
/* this program is very similar in design to cbm.c  */
// To run: ./imse <filename> mcmclen numcol
// file is assumed to contain a matrix of output from MCMC
// with mcmclen number of rows(chain length) and numcol number of columns (parameters)
#include<stdio.h>
#include<stdlib.h>
#include <stddef.h> 
#include <string.h>
//#define MATHLIB_STANDALONE 1
//#include <Rmath.h>

/* #define NUMIT 1000   */
/* #define NUMCOL 1324  */
#define VERBOSE 0 // 1=> print descriptions of output
/* function for computing sample covariance between two vectors of doubles */
/* IMPORTANT: NOTE THAT THIS IS THE UNBIASED ESTIMATE OF COVARIANCE (dividing by n-1) */
double cov(double *vec1,double *vec2,int len)
{
  double res;
  double mean1,mean2;
  int i;

  mean1=0.0;
  mean2=0.0;
  for (i=0;i<len;i++)
    {
      mean1=mean1+vec1[i];
      mean2=mean2+vec2[i];
/*       printf("%lf,%lf\n",vec1[i],vec2[i]); */
    }
  mean1=mean1/len;
  mean2=mean2/len;
/*   printf("mean1,mean2 = %lf,%lf\n",mean1,mean2); */

  res=0.0;
  for (i=0;i<len;i++)
    {
      res=res + (mean1-vec1[i])*(mean2-vec2[i]);
    }
  res=res/(len-1);

  printf("cov = %lf\n",res);
  return(res);
}
/* function for computing sample correlation between two vectors of doubles */
double cor(double *vec1,double *vec2,int len)
{
  double res;
  double mean1,mean2;
  double sd1,sd2,cov;
  int i,j;

  mean1=0.0;
  mean2=0.0;
  for (i=0;i<len;i++)
    {
      mean1=mean1+vec1[i];
      mean2=mean2+vec2[i];
    }
  mean1=mean1/len;
  mean2=mean2/len;

  sd1=0.0;
  sd2=0.0;
  for (i=0;i<len;i++)
    {
      sd1=sd1+(mean1-vec1[i])*(mean1-vec1[i]);
      sd2=sd2+(mean2-vec2[i])*(mean2-vec2[i]);
    }
  sd1=sd1/(len-1);
  sd1=sqrt(sd1);
  sd2=sd2/(len-1);
  sd2=sqrt(sd2);

  cov=0.0;
  for (i=0;i<len;i++)
    {
      cov=cov + (mean1-vec1[i])*(mean2-vec2[i]);
    }
  cov=cov/(len-1);

  res=cov/(sd1*sd2);

  return(res);
}

// assume that the vector values are g(x_1),..,g(x_n)
// and expectation of interest is E(g(x))
// n is the length of the vector, and vec is the vector
/* Note: we are using ipse (Geyer 1992), not imse (we do not worry about monotonicity, just positivity for simplicity)*/
int ipse(int n,double *vec,int bs,double *ipseres)
{
  double autocov;/*to hold autocovariance values */
  double *vec1,*vec2,*vec3; /* two vectors, both with same length */
  double prevgamma,currgamma;  /*gamma_j= autocov_2j + autocov_2j+1 */
  int ipseflag=1; /* if 0, then no longer positive sequence of gammas (stop), if 1, keep going */
  double ipsevar=1.0; /* this is the min. value for ipse variance */
  int j;
  int maxlag; 
  double est,serr,sigmahatsq; 
  int a,b,Nbm; // batch number and batch size, product
  int i,k;
  double *Y;
  int len;

  /* allocate memory */
  vec2=(double *) malloc(n*sizeof(double));
  vec3=(double *) malloc(n*sizeof(double));

  maxlag=10*log10(n);
  printf("maxlag=%d\n",maxlag);
  prevgamma=1.0;

  est=0.0;
  for (i=0;i<n;i++)
    est = est + vec[i];
  est=est/n; /* mean of samples */

  ipsevar=-1*cov(vec,vec,n); /* autocov(lag=0), i.e. variance */
  j=0;
  while ((ipseflag==1) && ((2*j)<maxlag)) /* while sequence is positive and before getting to maximum lag */
    {
      /* create the vectors for computing autocovariances */
      len=n-(2*j); /* n-lag */
      for (i=0;i<len;i++)
	{
	  vec2[i]=vec[i+(2*j)];/* vector with indices shifted by lag */
	}
      currgamma=cov(vec,vec2,len);

      len=n-(2*j+1); /* n-lag */
      for (i=0;i<len;i++)
	{
	  vec3[i]=vec[i+(2*j+1)]; /* vector with indices shifted by lag */
	}
      currgamma=currgamma +cov(vec,vec3,len); /* gamma = autocov(lag 2j) + autocov(lag 2j+1)*/

      if (currgamma<0.0) /*negative*/
	{
	  ipseflag=0; /* stop */
	}
      else
	{
	  ipsevar = ipsevar + 2*currgamma; 
	  ipseflag=1; /* keep going */
	}

      j=j+1; /* increment counter to get next pair of lag autocovariances */
    }
  
  if (j==0) /* in the special case that gamma_0 is non-positive */
    ipsevar=1.0; /* smallest possible value for imse var. */

  serr=sqrt(ipsevar/n);
  
  ipseres[0]=est;
  ipseres[1]=serr;
  
  return(1);
}

// assume that the vector values are g(x_1),..,g(x_n)
// and expectation of interest is E(g(x))
// n is the length of the vector, and vec is the vector
/* Note: imse (Geyer 1992), uses monotonicity and positivity*/
/* FIX THIS */
int imse(int n,double *vec,int bs,double *imseres)
{
  double autocov;/*to hold autocovariance values */
  double *vec1,*vec2,*vec3; /* two vectors, both with same length */
  double prevgamma,currgamma;  /*gamma_j= autocov_2j + autocov_2j+1 */
  int imseflag=1; /* if 0, then no longer positive sequence of gammas (stop), if 1, keep going */
  double imsevar=1.0; /* this is the min. value for imse variance */
  int j;
  int maxlag; 
  double est,serr,sigmahatsq; 
  int a,b,Nbm; // batch number and batch size, product
  int i,k;
  double *Y;
  int len;

  /* allocate memory */
  vec2=(double *) malloc(n*sizeof(double));
  vec3=(double *) malloc(n*sizeof(double));

  maxlag=10*log10(n);
  printf("maxlag=%d\n",maxlag);
  for (i=0;i<len;i++)
    {
      vec2[i]=vec[i+1];/* vector with indices shifted by lag */
    }
  prevgamma=cov(vec,vec,n) + cov(vec,vec2,n); /* initialize gamma (first gamma) for j=0 */

  est=0.0;
  for (i=0;i<n;i++)
    est = est + vec[i];
  est=est/n; /* mean of samples */

  imsevar=-1*cov(vec,vec,n); /* autocov(lag=0), i.e. variance */
  j=0;
  while ((imseflag==1) && ((2*j)<maxlag)) /* while sequence is positive and before getting to maximum lag */
    {
      /* create the vectors for computing autocovariances */
      len=n-(2*j); /* n-lag */
      for (i=0;i<len;i++)
	{
	  vec2[i]=vec[i+(2*j)];/* vector with indices shifted by lag */
	}
      currgamma=cov(vec,vec2,len);

      len=n-(2*j+1); /* n-lag */
      for (i=0;i<len;i++)
	{
	  vec3[i]=vec[i+(2*j+1)]; /* vector with indices shifted by lag */
	}
      currgamma=currgamma +cov(vec,vec3,len); /* gamma = autocov(lag 2j) + autocov(lag 2j+1)*/

      if ((currgamma<0.0) || (currgamma>prevgamma)) /*negative  OR non-monotone (latest gamma is larger than previous gamma) */
	{
	  imseflag=0; /* stop */
	}
      else
	{
	  imsevar = imsevar + 2*currgamma; 
	  prevgamma=currgamma; /* hold on to most recent gamma to check monotonicity next time */
	  imseflag=1; /* keep going */
	}

      j=j+1; /* increment counter to get next pair of lag autocovariances */
    }
  
  if (j==0) /* in the special case that gamma_0 is non-positive */
    imsevar=1.0; /* smallest possible value for imse var. */

  serr=sqrt(imsevar/n);
  
  imseres[0]=est;
  imseres[1]=serr;
  
  return(1);
}
int main(int argc, char *argv[]) 
{
  FILE *fs = NULL;
/*   double mcmc[NUMCOL][NUMIT]; */
/*   double X[NUMIT]; /\* each column (temporary) *\/ */
/*   double allres[NUMCOL][2]; /\* put together results from all columns *\/ */
  double **mcmc;
  double *X; /* each column (temporary) */
  double **allres; /* put together results from all columns */
  double imseres[2]; // keep batch means estimate and s.error
  int veclen;
  char *inputpath;
  int strlength;
  int i,j;
  
  int mcmclen, numcol;

  if (argc<4)
    {
      printf("Error ! Need filename from which to read MCMC output vector, length of Markov chain, number of parameters (columns).\n");
      printf("usage: %s <filename> mcmclen numcol\n",argv[0]);
      //      printf("See smcmc.README for details\n");
      exit(1);
    }

  else
    {
      strlength = strlen(argv[1]);
      inputpath = malloc(strlength*sizeof(char)+1); /* CHECK this added 1 to avoid strange errors*/
      sprintf(inputpath,"%s",argv[1]);

      mcmclen = atoi(argv[2]); 
      numcol = atoi(argv[3]); 
    }

  /* allocate memory according to length of MCMC run and number of columns */
  /* X[mcmclen] */
  X=(double *) malloc(mcmclen*sizeof(double));

  /* mcmc[numcol][mcmclen] */
  mcmc = (double **) malloc(numcol*sizeof(double));
  for (i=0;i<numcol;i++)
    mcmc[i] = (double *) malloc (mcmclen*sizeof(double));

  /* allres[numcol][2] */
  allres = (double **) malloc(numcol*sizeof(double));
  for (i=0;i<numcol;i++)
    allres[i] = (double *) malloc (2*sizeof(double));

  /* read in matrix */
  fs = fopen(inputpath,"r");
  if(fs==NULL) 
    { 
      fprintf(stderr,"\nCould not open input file %s .Exiting...\n",inputpath); 
      getchar(); 
      exit(0); 
    }

  /* read in as transpose (may be inefficient) */
  for (i=0;i<mcmclen;i++)
    for (j=0;j<numcol;j++)
      fscanf(fs,"%lf ",&mcmc[j][i]);

  /* compute the imse estimates for each column */
  for (j=0;j<numcol;j++)
    {
      for (i=0;i<mcmclen;i++)
	X[i]=mcmc[j][i];  /* X is now ith column of the matrix */
      imse(mcmclen,X,1,imseres); // compute s.error using batch means

      allres[j][0]=imseres[0];
      allres[j][1]=imseres[1];
    }
/*     printf("chain length=%d,batch means est=%f,serr=%f\n",veclen,imseres[0],imseres[1]); */
  for (j=0;j<numcol;j++)
    printf("%f %f\n",allres[j][0],allres[j][1]);
  
  return EXIT_SUCCESS;
}
