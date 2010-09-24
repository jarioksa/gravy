/*******************************************************
 * Hill scaling
 * Use Hill's 1st and 2nd scaling methods (yes, there are two of them!)
 * with observed gradients.
 * 
 * This file contains basic computation utilities: 
 * Driver is somewhere else...
 ******************************************************/

#include <math.h>  /* sqrt */
#include <stdio.h> /* debugging */

/* WA estimates of optimum and tolerance for species */

void GetWA (double *f, double *x, int *nnsp, int *nnst,  
	    double *spWA, double *spTol)
{
     int i, j, ij;
     double  sf, sfx, sfxx, tmp;
     int nsp, nst;
     nsp = *nnsp;
     nst = *nnst;

     ij = 0;
     for (j = 0; j < nsp; j++) {
	  sf = sfx = sfxx = 0.0;
	  for (i = 0; i < nst; i++) {
	       tmp = f[ij];
	       sf += tmp;
	       sfx += tmp*x[i];
	       sfxx += tmp*x[i]*x[i];
	       ij++;
	  }
	  spWA[j] = sfx/sf;
	  tmp = (sfxx-sfx*sfx/sf)/sf;
	  tmp = (tmp < 0.0) ? 0.0 : tmp;
	  spTol[j] = sqrt(tmp);
     }
}

/* Expands spWA and spTol so that spWA gives the correct variance to x */

void WAexpand (double *x, double *spWA, double *spTol, double *fidot, 
	      double *fdotj, double *fdotdot, int *nnsp, int *nnst)
{
     int i;
     double sx, sxx, Varx, VarWA, mean, c;
     int nst, nsp;
     nst = *nnst;
     nsp = *nnsp;

     /* Variance of x */

     for (i=0, sx=0.0, sxx=0.0 ; i<nst; i++) {
	  sx += fidot[i]*x[i];
	  sxx += fidot[i]*x[i]*x[i];
     }
     Varx = (sxx-sx*sx/(*fdotdot))/(*fdotdot);

     /* Variance of WA */

     for (i=0, sx=0.0, sxx=0.0; i<nsp; i++) {
	  sx += fdotj[i]*spWA[i];
	  sxx += fdotj[i]*spWA[i]*spWA[i];  
     }
     VarWA = (sxx-sx*sx/(*fdotdot))/(*fdotdot);
     mean = sx/(*fdotdot);

     /* c gives the ratio: that should be otherway round, so multiply */
     
     c = Varx/VarWA;
     for (i=0; i<nsp; i++) {
	  spWA[i] = c*(spWA[i]-mean)+mean;
	  spTol[i] *= sqrt(c);
     }
     /* I'm not sure whether spTol should be expanded or left like it
      * is, but simulations indicate that the above expansion gives
      * reasonable good estimates of the true Gaussian response,
      * although sligthly over-estimates the width. An alternative is
      * to leave spTol unexpanded, but this seems to give way too low
      * estimates of response widths.
      */
}

/* We need marginal totals of community data for WAexpand, which
   should evaluate the weighted variances.
*/

void Marginals(double *f, int *nnsp, int *nnst, 
	      double *fidot, double *fdotj, double *fdotdot)
{
     int i, j, ij;
     int nsp, nst;
     nsp = *nnsp;
     nst = *nnst;

     for (i=0; i<nsp; i++)
	  fdotj[i] = 0.0;
     for (i=0; i<nst; i++)
	  fidot[i] = 0.0;
     *fdotdot = 0.0;

     for (i=0, ij=0; i < nsp; i++) {
	  for (j = 0; j < nst; j++) {
	       fdotj[i] += f[ij];
	       fidot[j] += f[ij];
	       ij++;
	  }
     }

     for (i=0; i<nst; i++)
	  *fdotdot += fidot[i];
}

/* Hill had two scalings: h1 is the weighted mean WA-tolerance, h2 is
   the weighted squared difference of site scores and species within a
   site.  While h1 was described and discussed, h2 is actually used in
   DECORANA and CANOCO.
*/

void GetHill (double *f, double *spWA, double *spTol, double *x, 
	     int *nnsp, int *nnst, double *h1, double *h2)
{
     int i, j, ij;
     double sf, sfx, sfxx, sft, diff, tmp;
     int nsp, nst;
     nsp = *nnsp;
     nst = *nnst;

     /* Original DECORANA code estimated diff as x[i]-spWA[j], and
	x[i] was defined to be the WA of spWA[j]'s, hence sfx==0.
	These assumptions do not hold for observed gradients, and
	therefore I estimate h2 as the weighted variance of spWA
	within a site.
     */
     for (j=0; j < nst; j++ ) {
	  sf = sfx = sfxx = sft = 0.0;
	  ij = j;
	  for (i = 0; i < nsp; i++) {
	       tmp = f[ij];
	       sf += tmp;
	       diff = spWA[i];
	       sfx += tmp*diff;
	       sfxx += tmp*diff*diff;
	       sft += tmp*spTol[i];
	       ij += nst;
	  }
	  h1[j] = sft/sf;
	  h2[j] = (sfxx-sfx*sfx/sf)/sf;
     }
}

/* Divides x into nseg segments and gets the sum and count of hills' 
   on these segment pieces. */

void segmnt(double *h1, double *h2, double *x, int nst, int nseg,
	   double *zv1, double *zv2, double *zn)
{
     double axmax, axmin, axbit;
     int i, k;

     /* Find gradient extremes and zero z-vectors */

     axmax = axmin = x[0];
     for (i=1; i<nst; i++) {
	  axmax = (x[i] > axmax) ? x[i] : axmax ;
	  axmin = (x[i] < axmin) ? x[i] : axmin ;
     }

     for (i=0; i<nseg; i++)
	  zv1[i] = zv2[i] = zn[i] = 0.0;

     /* Chop into segments and sum */

     axbit = (axmax-axmin)/nseg;
  
     for (i=0; i<nst; i++) {
	  k = (x[i]-axmin)/axbit;
	  if (k<0)
	       k=0;
	  if (k>nseg-1)
	       k=nseg-1;
	  zv1[k] += h1[i];
	  zv2[k] += h2[i];
	  zn[k] += 1.0;
     }
}

/* The famous smoothing routine for detrending.
   This must be called separately for each z-vector, and hopefully
   it operates all similarly...
*/

void smooth (double *z, int nseg)
{
     int sweeps, k;
     double z1, z2, z3;
  
     sweeps=1;

     do {
	  z2 = z[0];
	  z3 = z[1];
	  if (z[3] <= 0.0)
	       sweeps = 0;
	  z[0] = 0.75*z2 + 0.25*z3;
	  for (k=2; k<nseg; k++) {
	       z1 = z2;
	       z2 = z3;
	       z3 = z[k];
	       if (z3 <= 0.0)
		    sweeps = 0;
	       z[k-1] = 0.5*(z2 + 0.5*(z1+z3));
	  }
	  z[nseg-1] = 0.25*z2 + 0.75*z3;
	  sweeps++;
     } while (sweeps <= 4);
}

/* STRTCH() is the basic driver routine for non-linear rescaling in
   DECORANA (written by Dr. Mark O. Hill). hillstrtch() is a C routine
   modelled to mimic the DECORANA routines, with all ad hocs of the
   original.  However, a part of stretching is done in separate
   functions above.  The C++ comments on the right refer to the
   original Fortran loops.
*/


#include <stdio.h> /* Debug */

#include <R.h> /* R_alloc */

/* Arbitrary constants, as used in DECORANA/ CANOCO */

#define SEGMIN 10
#define SEGMAX 45
#define SEGODD 20
#define TIMES 2
#define SHORT 0.0

void hillstrtch(double *f, double *x, int *nnsp, int *nnst, int *nresc, 
		double *h1, double *h2, double *x2tic, double *zv1, double *zv2, double *zn)
{
     int cycle, i, k, nseg, bigwheel, x2n=SEGODD+1;
     double zvsum, axmin, axmax, along, sd, coeff, axbit, az;
     double zwork[SEGMAX+1];
/*      double zn[SEGMAX];  */
     double *spWA, *spTol, *fidot, *fdotj, fdotdot;
     int nsp, nst, resc;

     nsp = *nnsp;
     nst = *nnst;
     resc = *nresc;

     spWA =  (double *) R_alloc(nsp, sizeof(double));
     spTol = (double *) R_alloc(nsp, sizeof(double));
     fidot = (double *) R_alloc(nst, sizeof(double));
     fdotj = (double *) R_alloc(nsp, sizeof(double));

     /* Make the axis start from zero, like it would start in DECORANA. */
 
     axmin = x[0];
     axmax = x[0];
     for (i=1; i<nst; i++) {
	  if (x[i] < axmin)
	       axmin = x[i];
	  else if (x[i] > axmax)
	       axmax = x[i];
     }
     for (i=0; i<nst; i++) {
	  x[i] = x[i] - axmin;
     }

     /* x2tic is regularly spaced on the original gradient. It is treated
	similarly as the gradient to be used in interpreting rescaling.
     */

     axbit = (axmax-axmin)/(x2n-1.0);
     for (i=0; i<x2n; i++)
	  x2tic[i] = i*axbit;

     /* Marginal totals do not change in rescaling: so this is a good
	place of getting them */

     Marginals(f, nnsp, nnst, fidot, fdotj, &fdotdot);

     /* Big Wheel: The Hill rescaling routine */

     for (bigwheel=0; bigwheel < resc; bigwheel++) { 
	  for (cycle=0; cycle < TIMES; cycle++) {    // 200

	       /* Get or update WA, Tol and Hill measures */
	       GetWA (f,x, nnsp, nnst, spWA, spTol);
	       WAexpand (x,spWA, spTol, fidot, fdotj, &fdotdot, nnsp, nnst);
	       GetHill(f, spWA, spTol, x, nnsp, nnst, h1, h2);
    
	       /* Odd numbered pass of rescaling */
	       nseg = SEGODD;
	       segmnt (h1,h2,x,nst,nseg,zv1,zv2,zn);
	       smooth (zv1,nseg);
	       smooth (zv2,nseg);
	       smooth (zn,nseg);

	       for (i=0,zvsum=0.0; i<nseg; i++) {    // 50
		    zvsum += zv2[i]/zn[i];
	       }
	       sd = sqrt(zvsum/nseg);
	       for (i=0, along=0.0; i<nst; i++) {   // 60
		    x[i] /= sd;
		    along = (along < x[i]) ? x[i] : along ;
	       }

	       /* Same for x2tic */
	       for (i=0; i<x2n; i++)
		    x2tic[i] /= sd;

	       for (i=0; i<nsp; i++) {  // 70
		    spTol[i] /= sd;
		    spWA[i] /= sd;
	       }

	       /* This will update the statistics by sd as well, not
		  only the raw data. This was missing in the original STRTCH code. */
	       coeff = sd*sd;
	       for (i=0; i<nseg; i++) {
		    zv1[i] /= sd;
		    zv2[i] /= coeff;
	       }
	       for (i=0; i<nst; i++) {
		    h1[i] /= sd;
		    h2[i] /= coeff;
	       }

	       /* This first part with NSEG segments is done twice, but 
		  quit the rest on the second cycle.
	       */
	       if (cycle==1) 
		    break;

	       /* Quit here if gradient too short, default: never*/
	       if (along < SHORT)
		    return; 

	       /* Even numbered pass of rescaling */
	       nseg = along*5.0 + 1;
	       if (nseg < SEGMIN)
		    nseg = SEGMIN;
	       if (nseg > SEGMAX)
		    nseg = SEGMAX;

	       segmnt (h1,h2,x,nst,nseg,zv1,zv2,zn);
	       /*   smooth (zv1,nseg); */
	       /* The above is never used, so why compute? */
	       smooth (zv2,nseg);
	       smooth (zn,nseg);

	       for (i=0,zvsum=0.0; i<nseg; i++) {   // 100
		    zv2[i] = 1.0/sqrt(0.2/along + zv2[i]/zn[i]);
		    zvsum += zv2[i];
	       }

	       coeff = along/zvsum;
	       for (i=0; i<nseg; i++) {   // 110
		    zv2[i] *= coeff;
	       }

	       zwork[0] = 0.0;
	       for (i=0, az=0; i<nseg; i++) {   // 120
		    az += zv2[i];
		    zwork[i+1] = az;
	       }

	       /* Finally the proper rescaling by segments.  The
		  original code makes this rescaling to species scores
		  (y) so that site scores still are weighted averages
		  of species scores. (A minor anomaly is that in some
		  cases this may cause change of ordering os site
		  scores after rescaling.) However, we want to keep
		  site scores as a rescaled observed gradient, and
		  species scores are only secondary. In the final
		  solution of CA, site and species scores naturally
		  are proportional to weighted averages of each other,
		  but that doesn't concern us here.
	       */

	       axbit = along/nseg;

	       for (i=0; i<nst; i++) {   // 130
		    k = x[i]/axbit;
		    if (k<0)
			 k = 0;
		    if (k>=nseg)
			 k = nseg-1;
		    x[i] = zwork[k] + zv2[k]*(x[i]/axbit-k);
	       }

	       /* Same for x2tic, but without checking k */
	       for (i=0; i<x2n; i++) {
		    k = x2tic[i]/axbit;
		    if (k>=nseg)
			 k = nseg-1;
		    x2tic[i] = zwork[k] + zv2[k]*(x2tic[i]/axbit-k);
	       }

	  } /* End the pass (cycle) 200 */
     } /* End the all resc rescaling cycles */
}

/* Get the Hill indices of beta diversity with a segmented smooth without rescaling */

void hill0(double *f, double *x, int *nnsp, int *nnst, double *h1, double *h2, 
	   double *zv1, double *zv2)
{
     double *spWA, *spTol, *fidot, *fdotj, fdotdot;
     double zn[SEGODD];
     int i;

     spWA =  (double *) R_alloc(*nnsp, sizeof(double));
     spTol = (double *) R_alloc(*nnsp, sizeof(double));
     fidot = (double *) R_alloc(*nnst, sizeof(double));
     fdotj = (double *) R_alloc(*nnsp, sizeof(double));

     GetWA (f,x, nnsp, nnst, spWA, spTol);
     Marginals(f, nnsp, nnst, fidot, fdotj, &fdotdot);
     WAexpand (x,spWA, spTol, fidot, fdotj, &fdotdot, nnsp, nnst);
     GetHill(f, spWA, spTol, x, nnsp, nnst, h1, h2);
     segmnt (h1,h2,x,*nnst,SEGODD,zv1,zv2,zn);
     smooth (zv1,SEGODD);
     smooth (zv2,SEGODD);
     smooth (zn,SEGODD);
     for (i=0; i<SEGODD; i++) {
	  zv1[i] = zv1[i]/zn[i];
	  zv2[i] = zv2[i]/zn[i];
     }
}
