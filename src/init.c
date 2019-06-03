#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Fortran calls */
extern void F77_NAME(rs_bos)(double *res, double *dat, int *nodvec, double *col, int *m, int *n, int *minbucket, int *crit);
extern void F77_NAME(rs_boscat)(double *res, double *dat, int *nodvec, int *m, int *n, double *trx, double *trx2, int *mm, int *minbucket, int *crit);
extern void F77_NAME(rs_boscats)(double *res, double *dat, int *nodvec, double *col, int *m, int *n, int *minbucket, int *crit);
extern void F77_NAME(rs_cor)(double *c, double *x, double *y, int *n);
extern void F77_NAME(rs_cov)(double *c, double *x, double *y, double *mx, double *my, int *n);
extern void F77_NAME(rs_mean)(double *m, double *x, int *n);
extern void F77_NAME(rs_resid)(double *r, double *dat, int *m, int *n);
extern void F77_NAME(rs_rsq)(double *r, double *dat, int *m, int *n);
extern void F77_NAME(rs_splitc)(double *fvec, int *predtree, int *nodemat2, double *index2, double *datarta, double *datanum, int *typ, int *npt, int *nnm2, int *mnm2, int *ni2, int *nrta, int *mrta, int *nnum, int *mnum, int *ntyp, int *mb, int *critn);
extern void F77_NAME(rs_stdev)(double *s, double *x, double *m, int *n);

static const R_FortranMethodDef FortranEntries[] = {
    {"rs_bos",     (DL_FUNC) &F77_NAME(rs_bos),      8},
    {"rs_boscat",  (DL_FUNC) &F77_NAME(rs_boscat),  10},
    {"rs_boscats", (DL_FUNC) &F77_NAME(rs_boscats),  8},
    {"rs_cor",     (DL_FUNC) &F77_NAME(rs_cor),      4},
    {"rs_cov",     (DL_FUNC) &F77_NAME(rs_cov),      6},
    {"rs_mean",    (DL_FUNC) &F77_NAME(rs_mean),     3},
    {"rs_resid",   (DL_FUNC) &F77_NAME(rs_resid),    4},
    {"rs_rsq",     (DL_FUNC) &F77_NAME(rs_rsq),      4},
    {"rs_splitc",  (DL_FUNC) &F77_NAME(rs_splitc),  18},
    {"rs_stdev",   (DL_FUNC) &F77_NAME(rs_stdev),    4},
    {NULL, NULL, 0}
};

void R_init_stima(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
