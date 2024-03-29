#include <stdio.h>
#include <string.h>
#include "erl_nif.h"
#include <cblas.h>
//#include <openblas-serial/cblas.h>


typedef struct {
    const double* data;
    int stride;
    int n;
} vector;


int debug_write(const char*, ...);
ERL_NIF_TERM daxpy (ErlNifEnv*, int, const ERL_NIF_TERM[]);
ERL_NIF_TERM wait_c(ErlNifEnv*, int, const ERL_NIF_TERM []);
