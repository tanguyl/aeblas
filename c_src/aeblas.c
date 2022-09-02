#include "aeblas.h"
#include <sys/time.h>
// Another Erlang Blas.


ERL_NIF_TERM atom_ok;

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

 
ERL_NIF_TERM wait_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    struct timeval stop, start;
    gettimeofday(&start, NULL);

    while(1){
        gettimeofday(&stop, NULL);
        int micro = (stop.tv_sec - start.tv_sec) * 1000000 + stop.tv_usec - start.tv_usec;

        if(micro > 1000)
            break;
    }

    return atom_ok;
}




// ----------------------------------------------------------------------
// ----------------------------------------------------------------------

ErlNifFunc nif_funcs[] = {
    {"daxpy_nif", 6,  daxpy},
    {   "wait_c", 1, wait_c}
};


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,ERL_NIF_TERM load_info)
{return 0;}

static void unload(ErlNifEnv* env, void* priv_data)
{}

ERL_NIF_INIT(aeblas,nif_funcs,load,NULL,upgrade,unload)