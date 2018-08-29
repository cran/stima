#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(rs_bos)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(rs_boscat)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(rs_boscats)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(rs_cor)(void *, void *, void *, void *, void *);
extern void F77_NAME(rs_cov)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(rs_mean)(void *, void *, void *, void *);
extern void F77_NAME(rs_resid)(void *, void *, void *, void *, void *);
extern void F77_NAME(rs_rsq)(void *, void *, void *, void *, void *);
extern void F77_NAME(rs_splitc)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(rs_stdev)(void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"rs_bos",     (DL_FUNC) &F77_NAME(rs_bos),      9},
    {"rs_boscat",  (DL_FUNC) &F77_NAME(rs_boscat),  11},
    {"rs_boscats", (DL_FUNC) &F77_NAME(rs_boscats),  9},
    {"rs_cor",     (DL_FUNC) &F77_NAME(rs_cor),      5},
    {"rs_cov",     (DL_FUNC) &F77_NAME(rs_cov),      7},
    {"rs_mean",    (DL_FUNC) &F77_NAME(rs_mean),     4},
    {"rs_resid",   (DL_FUNC) &F77_NAME(rs_resid),    5},
    {"rs_rsq",     (DL_FUNC) &F77_NAME(rs_rsq),      5},
    {"rs_splitc",  (DL_FUNC) &F77_NAME(rs_splitc),  19},
    {"rs_stdev",   (DL_FUNC) &F77_NAME(rs_stdev),    5},
    {NULL, NULL, 0}
};

void R_init_stima(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
