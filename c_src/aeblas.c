#include "aeblas.h"
// Another Erlang Blas.

//Used for debug purpose.
//Likely thread unsafe.
//Usage: debug_write("A double: %lf, an int:%d", double_val, int_val);
int debug_write(const char* fmt, ...){
    FILE* fp = fopen("debug.txt", "a");
    va_list args;

    va_start(args, fmt);
    vfprintf(fp, fmt, args);
    va_end(args);

    fclose(fp);
    return 1;
}

// Translate a erlang vector tupple to a c struct.
// Can be fred using enif_free(vector).
ERL_NIF_TERM vector_etc(ErlNifEnv* env, ERL_NIF_TERM tupple, vector* v){
    int arity;
    const ERL_NIF_TERM* terms;

     if(!enif_get_tuple(env, tupple, &arity, &terms) || arity != 3)
        return 0;
    
    ErlNifBinary bin;
    int stride;
    int n;

    if(!enif_inspect_binary(env, terms[1], &bin)
        || !enif_get_int(env, terms[2], &n)
        || !enif_get_int(env, terms[3], &stride)
    )
        return enif_make_badarg(env);

    v->data   = (double*) bin.data;
    v->n      = n;
    v->stride = stride;

    
    return 1;
}


ERL_NIF_TERM axpy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    vector x,y;
    ErlNifBinary res;
    double alpha;

    debug_write("We are here\n");

    if(!enif_get_double(env, argv[0], &alpha)
        || !vector_etc(env, argv[1], &x)
        || !vector_etc(env, argv[2], &y)
        || !debug_write("Vectors read\n")
        || !enif_alloc_binary(x.n*sizeof(double), &res)
    )   return enif_make_badarg(env);

    cblas_dcopy(y.n, y.data, y.stride, (double*) res.data, 1);
    cblas_daxpy(x.n, alpha, x.data, x.stride, (double*) res.data, 1);

    return enif_make_tuple3(
                env,
                enif_make_atom(env,"vector"),
                enif_make_binary(env, &res),
                enif_make_int(env, x.stride)
    );
}

ERL_NIF_TERM daxpy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    int n, stride_x, stride_y;
    double alpha;
    ErlNifBinary x, y, res;

    if(!enif_get_int(env, argv[0], &n)
        || !enif_get_double(env, argv[1], &alpha)
        || !enif_inspect_binary(env, argv[2], &x)
        || !enif_get_int(env, argv[3], &stride_x)
        || !enif_inspect_binary(env, argv[4], &y)
        || !enif_get_int(env, argv[5], &stride_y)
        || !enif_alloc_binary(n*sizeof(double), &res)
    )   return enif_make_badarg(env);

    double* ptr_x = (double*) x.data;
    double* ptr_y = (double*) y.data;
    double* ptr_r = (double*) res.data;

    cblas_dcopy(n, ptr_y, stride_y, ptr_r, 1);
    cblas_daxpy(n, alpha, ptr_x, stride_x, ptr_r, 1);

    enif_consume_timeslice(env, 100);

    return enif_make_binary(env, &res);
}

 /*	
character 	TRANSA,
character 	TRANSB,
integer 	M,
integer 	N,
integer 	K,
double precision 	ALPHA,
double precision, dimension(lda,*) 	A,
integer 	LDA,
double precision, dimension(ldb,*) 	B,
integer 	LDB,
double precision 	BETA,
double precision, dimension(ldc,*) 	C,
integer 	LDC 
)	
*/

/*
ERL_NIF_TERM dgemm(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    char transA[2], transB[2];
    int M,N,K, LDA,LDB,LDC;
    double alpha, *A, *B, *C, *R;
    ErlNifBinary A_bin, B_bin, C_bin, R_bin;


    if(!enif_get_atom(env, argv[0], transA, 1, ERL_NIF_LATIN1)
        || !enif_get_atom(env, argv[1], transB, 1, ERL_NIF_LATIN1)
        || !enif_get_int(env, argv[2], &M)
        || !enif_get_int(env, argv[3], &N)
        || !enif_get_int(env, argv[4], &K)
        || !enif_get_double(env, argv[5], &alpha)
        || !enif_inspect_binary(env, argv[6], &A_bin)
        || !enif_get_int(env, argv[7], &LDA)
        || !enif_inspect_binary(env, argv[8], &B)
        || !enif_get_int(env, argv[9], &LDB)
        || !enif_inspect_binary(env, argv[10], &C_bin)
        || !enif_get_int(env, argv[11], &LDC)    
        || !enif_alloc_binary(sizeof(double)*M*K, &R_bin)
    )
        return enif_make_badarg(env);
    
    C = (double*) C_bin.data;
    R = (double*) R_bin.data;

    cblas_dcopy(M*N, C, 1, R, 1);

    A = (double*) A_bin.data;
    B = (double*) B_bin.data;

    cblas_dgemm()
}
*/




// ----------------------------------------------------------------------
// ----------------------------------------------------------------------

ErlNifFunc nif_funcs[] = {
    {"daxpy_nif", 6, daxpy, 0}
};


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{return 0;}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,ERL_NIF_TERM load_info)
{return 0;}

static void unload(ErlNifEnv* env, void* priv_data)
{}

ERL_NIF_INIT(aeblas,nif_funcs,load,NULL,upgrade,unload)