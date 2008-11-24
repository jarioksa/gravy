/*
  Fitted values and derivatives at a single gradient point
*/

/* Fitted values with HOF models */

/* I used to have MAXDOUBLE from values.h, but this header file
   is empty in Windows gcc MinGW32, and so I define it from R.h.
*/

#include <math.h>
#include <R.h>
#define MAXDOUBLE DOUBLE_XMAX
#define MAXPAR (log(MAXDOUBLE-1.0))

/*
  Model names: These conflict with R.h, and must be #define'd
  after #include'ing R.h
*/

#define V 5
#define IV 4
#define III 3
#define II 2
#define I 1

int HOF(int *Model, double *p, double *x, double *fit)
{
     double t1, t2, t4, t5, t6;
     double a, b, c, d;

     a = p[0];
     b = p[1];
     c = p[2];
     d = p[3];
     t1 = 0.0;
     t2 = 1.0;
     t4 = 0.0;
     t6 = 1.0;
     switch (*Model) {
     case V:
     case IV:
	  t1 = -d*(*x);
     case III:
	  t2 = c+t1;
	  if (t2 > MAXPAR)
	       t2 = MAXPAR;
	  t2 = 1.0/(1+exp(t2));
     case II:
	  t4 = b*(*x);
     case I:
	  t5 = a+t4;
	  if (t5 > MAXPAR)
	       t5 = MAXPAR;
	  t5 = 1+exp(t5);
	  t6 = t2/t5;
	  *fit = t6;
	  break;
     default:
	  return -1;
     }
     return 0;
}

#undef MAXPAR

/* Returns derivative of HOF (Value of HOF needed!) */

#define MAXPAR (log(MAXDOUBLE))

int dHOF(int *Model, double *p, double *x, double *f, double *der) 
{
     double t0, t1, t2, t3;
     double a, b, c, d;
     double f2;

     f2 = (*f)*(*f);

     a = p[0];
     b = p[1];
     c = p[2];
     d = p[3];
     t0=t1=t2=t3=0.0;

     switch(*Model) {
     case I:
	  return 0.0;
	  break;
     case V:
	  t3 = a+c+(b-d)*(*x);
	  if (t3 > MAXPAR)
	       t3 = MAXPAR;
	  t3 = (d-b)*f2*exp(t3);
     case IV:
	  t2 = c-d*(*x);     /* Model IV: c==d, Model V: c!=d */
	  if (t2 > MAXPAR)
	       t2 = MAXPAR;
	  t2 = d*f2*exp(t2);
	  break;
     case III:
	  t2 = a+c+b*(*x);
	  if (t2 > MAXPAR)
	       t2 = MAXPAR;
	  t2 = -b*f2*exp(t2);
	  break;
     case II:
	  break;
     }
     /* Models II,III,IV & V have this in common */
     t1 = a+b*(*x);
     if (t1 > MAXPAR)
	  t1 = MAXPAR;
     t1 = -b*f2*exp(t1);

     *der = t1+t2+t3;
     return 0;
}

#undef MAXPAR

      

