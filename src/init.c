#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void dHOF(void *, void *, void *, void *, void *);
extern void hill0(void *, void *, void *, void *, void *, void *, void *, void *);
extern void hillstrtch(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void HOF(void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"dHOF",       (DL_FUNC) &dHOF,        5},
    {"hill0",      (DL_FUNC) &hill0,       8},
    {"hillstrtch", (DL_FUNC) &hillstrtch, 11},
    {"HOF",        (DL_FUNC) &HOF,         4},
    {NULL, NULL, 0}
};

void R_init_gravy(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
