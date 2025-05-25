#include <R.h>
#include <Rinternals.h>

// Example implementation
SEXP foo_process(SEXP x, SEXP y) {
  R_len_t n = length(x);
  SEXP out = PROTECT(allocVector(REALSXP, n));
  double *dx = REAL(x), *dy = REAL(y), *do_ = REAL(out);
  for (R_len_t i = 0; i < n; i++) {
    do_[i] = dx[i] + dy[i];
  }
  UNPROTECT(1);
  return out;
}
