// src/init.c
#include <stddef.h>           // for NULL 
#include <R.h>                // basic R types and macros
#include <Rinternals.h>       // defines SEXP, R_len_t, etc. 
#include <R_ext/Rdynload.h>   // defines R_CallMethodDef & R_registerRoutines 

// Forward declaration of your C function
extern SEXP foo_process(SEXP, SEXP);

// The table of routines to register
static const R_CallMethodDef CallEntries[] = {
  {"foo_process", (DL_FUNC)&foo_process, 2},  // name, pointer, # args 
  {NULL,         NULL,               0}
};

// Initialization: register routines & disable dynamic symbol lookup
void R_init_tabela(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);  // register with R 
  R_useDynamicSymbols(dll, FALSE);                         // disallow .Call by symbol name
}
