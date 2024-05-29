/* Author: Murali Haran, Statistics Department, Penn State University */
/* same as cbm.c except only have function (no main program) for use with other C or R programs */
#include<stdio.h>
#include<stdlib.h>
#include <stddef.h> 
#include <string.h>
//#define MATHLIB_STANDALONE 1
//#include <Rmath.h>

#define VERBOSE 0 // 1=> print descriptions of output

/* assume that the vector values are g(x_1),..,g(x_n) */
/* and expectation of interest is E(g(x)) */
/* n is the length of the vector, and vec is the vector  */
/* bs is the batchsize. Default is sqrt(n), if bs==0, use cuberoot of n */
int cbm(int n,double *vec,int bs,double *bmres)
{
  double est,serr,sigmahatsq; 
  int a,b,Nbm; // batch number and batch size, product
  int i,k;
  double *Y;

  if (bs) // if sqroot (default)
    {   
      b=(int ) sqrt(n);
      a=n/b; // use 'div' operator
    }
  else //cuberoot
    {   
      b=(int ) pow(n,1/3);
      a=n/b; // use 'div' operator
    }

  Y=(double *)malloc(a*sizeof(double)); //memory allocation for batch means

  // computing batch means according to batch sizes
  est=0.0;
  for (k=0;k<a;k++) 
    {
      Y[k]=0.0;
      for (i=(k*b);i<=(k*b+b-1);i++)
	Y[k] = Y[k] + vec[i];
      est=est + Y[k];
      Y[k]=Y[k]/b; 
    }

  est=est/(a*b); // grand mean
  //  printf("est=%f\n",est);

  sigmahatsq=0.0;
  for (k=0;k<a;k++) 
    sigmahatsq=sigmahatsq+(Y[k]-est)*(Y[k]-est);

  sigmahatsq=sigmahatsq*b/(a-1);
  serr=sqrt(sigmahatsq/n);

  bmres[0]=est;
  bmres[1]=serr;

  return(1);
}
