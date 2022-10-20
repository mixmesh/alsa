//
// ALSA Sample buffer library mixing/resample/reformat
//
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <alsa/asoundlib.h>

#include "erl_nif.h"
#include "erl_driver.h"

//#define DEBUG
//#define NIF_TRACE

#include "fft.h"
#include "format.h"
#include "atom_hash.h"
#include "reformat.h"

#define MAX_CHANNELS 8

#define UNUSED(a) ((void) a)

#include "debug.h"
#define INFOF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#define ERRORF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#define BADARG(env) enif_fprintf(stderr, "%s: badarg line=%d\r\n", __FILE__, __LINE__), enif_make_badarg((env))

#define UNUSED(a) ((void) a)

#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

// require env in context (ugly)
#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define LOAD_ATOM_STRING(name,string)		\
    atm_##name = enif_make_atom(env,string)


static int load(ErlNifEnv* env, void** priv_data,
		       ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data,
			  void** old_priv_data,
		       ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(undefined);
DECL_ATOM(enoent);
DECL_ATOM(true);
DECL_ATOM(false);

// format - use SND_FORMAT_LIST to declare all atom formats
#undef SND_FORMAT
#define SND_FORMAT(a,f) DECL_ATOM(a);
SND_FORMAT_LIST

// Dirty optional since 2.7 and mandatory since 2.12
#if (ERL_NIF_MAJOR_VERSION > 2) || ((ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION >= 7))
#ifdef USE_DIRTY_SCHEDULER
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(0)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#endif
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#endif

#define NIF_LIST \
    NIF("new", 1,  nif_new)				\
    NIF("clear", 1,  nif_clear)				\
    NIF("size", 1,  nif_size)				\
    NIF("resize", 2,  nif_resize)			\
    NIF("fft", 4,  nif_fft)				\
    NIF("rfft", 4, nif_rfft)				\
    NIF("ifft", 3, nif_ifft)				\
    NIF("set_rectangular", 1, nif_set_rectangular)	\
    NIF("set_bartlett", 1, nif_set_bartlett)		\
    NIF("set_hanning", 1, nif_set_hanning)		\
    NIF("set_hamming", 1, nif_set_hamming)		\
    NIF("set_blackman", 1, nif_set_blackman)		\
    NIF("set_blackman_harris", 1, nif_set_blackman_harris) \
    NIF("set_custom", 5, nif_set_custom)

//    NIF("phase", 2, nif_phase)
//    NIF("log_magnitue", 2, nif_log_magnitude)
//    NIF("magnitude", 2, nif_magnitude)


ErlNifResourceType *fft_r;

#undef SND_FORMAT
#define SND_FORMAT(atm, form) HELEM(ATOM(atm), form),
helem_t format_elems[] =
{
    SND_FORMAT_LIST
    { .next = NULL, .atm_ptr = NULL, .enm = 0, .hval = 0}    
};

#define FORMAT_HASH_SIZE (2*(sizeof(format_elems)/sizeof(helem_t))+1)

typedef struct {
    helem_t* format_hash[FORMAT_HASH_SIZE];
} nif_ctx_t;

// Declare all nif functions
#undef NIF
#ifdef NIF_TRACE
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]); \
    static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#else
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#endif

NIF_LIST

#undef NIF
#ifdef NIF_TRACE
#define NIF(name,arity,func) NIF_FUNC(name, arity, trace##_##func##_##arity),
#else
#define NIF(name,arity,func) NIF_FUNC(name, arity, func),
#endif

static ErlNifFunc nif_funcs[] =
{
    NIF_LIST
};

static int get_size_t(ErlNifEnv* env, ERL_NIF_TERM arg, size_t* sizep)
{
    unsigned long sz;
    if (enif_get_ulong(env, arg, &sz)) {
	*sizep = sz;
	return 1;
    }
    return 0;
}

static int get_number(ErlNifEnv* env, ERL_NIF_TERM arg, double* num)
{
    long value;
    if (enif_get_long(env, arg, &value)) {
	*num = (double) value;
	return 1;
    }
    return enif_get_double(env, arg, num);
}


static int get_format(ErlNifEnv* env, ERL_NIF_TERM arg,
		      snd_pcm_format_t* format_ptr)
{
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    int val;

    if (enif_is_atom(env, arg)) {
	if ((val = lookup_atom(ctx->format_hash, FORMAT_HASH_SIZE, arg)) < 0)
	    return 0;
	*format_ptr = val;
	return 1;
    }
    else if (enif_is_number(env, arg)) {
	int format;
	if (!enif_get_int(env, arg, &format))
	    return 0;
	if ((format < 0) || (format > SND_PCM_FORMAT_LAST))
	    return 0;
	*format_ptr = format;
	return 1;
    }
    return 0;
}

static ERL_NIF_TERM nif_new(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    fft_t* fp = enif_alloc_resource(fft_r, sizeof(fft_t));
    ERL_NIF_TERM term;
    size_t size;

    if (!get_size_t(env, argv[0], &size))
	return enif_make_badarg(env);
    
    fft_init(fp, size);
    term = enif_make_resource(env, fp);
    enif_release_resource(fp);
    return term;
}

static ERL_NIF_TERM nif_clear(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    fft_t* fp;
    if (!enif_get_resource(env, argv[0], fft_r, (void **)&fp))
	return enif_make_badarg(env);
    fft_clear(fp);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_size(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    fft_t* fp;
    if (!enif_get_resource(env, argv[0], fft_r, (void **)&fp))
	return enif_make_badarg(env);
    return enif_make_ulong(env, (unsigned long) fp->n);
}

static ERL_NIF_TERM nif_resize(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    fft_t* fp;
    size_t size;    
    if (!enif_get_resource(env, argv[0], fft_r, (void **)&fp))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[1], &size))
	return enif_make_badarg(env);
    fft_resize(fp, size);
    return ATOM(ok);    
}

static ERL_NIF_TERM nif_set_rectangular(ErlNifEnv* env, int argc,
					const ERL_NIF_TERM argv[])
{
    fft_t *fp;
    if (!enif_get_resource(env, argv[0], fft_r, (void **)&fp))
	return enif_make_badarg(env);
    fft_set_rectangular(fp);
    return ATOM(ok);
} 

static ERL_NIF_TERM nif_set_bartlett(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    fft_t *fp;
    if (!enif_get_resource(env, argv[0], fft_r, (void **)&fp))
	return enif_make_badarg(env);
    fft_set_bartlett(fp);
    return ATOM(ok);
}    

static ERL_NIF_TERM nif_set_hanning(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    fft_t *fp;
    if (!enif_get_resource(env, argv[0], fft_r, (void **)&fp))
	return enif_make_badarg(env);
    fft_set_hanning(fp);
    return ATOM(ok);
}    

static ERL_NIF_TERM nif_set_hamming(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    fft_t *fp;
    if (!enif_get_resource(env, argv[0], fft_r, (void **)&fp))
	return enif_make_badarg(env);
    fft_set_hamming(fp);
    return ATOM(ok);
}    

static ERL_NIF_TERM nif_set_blackman(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    fft_t *fp;
    if (!enif_get_resource(env, argv[0], fft_r, (void **)&fp))
	return enif_make_badarg(env);
    fft_set_blackman(fp);
    return ATOM(ok);
}    

static ERL_NIF_TERM nif_set_blackman_harris(ErlNifEnv* env, int argc,
					    const ERL_NIF_TERM argv[])
{
    fft_t *fp;
    if (!enif_get_resource(env, argv[0], fft_r, (void **)&fp))
	return enif_make_badarg(env);
    fft_set_blackman_harris(fp);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_set_custom(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    fft_t *fp;
    double c0, c1,c2, c3;
    if (!enif_get_resource(env, argv[0], fft_r, (void **)&fp))
	return enif_make_badarg(env);
    if (!get_number(env, argv[1], &c0))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &c1))
	return enif_make_badarg(env);
    if (!get_number(env, argv[3], &c2))
	return enif_make_badarg(env);
    if (!get_number(env, argv[4], &c3))
	return enif_make_badarg(env);
    fft_set_custom(fp, c0, c1, c2, c3);
    return ATOM(ok);
}    


// fft(FFT, Format, Channels, Data) -> [F1,..Fn]
// output if native complex float (real imag real imag ... )
static ERL_NIF_TERM nif_fft(ErlNifEnv* env, int argc,
			    const ERL_NIF_TERM argv[])
{
    fft_t* fp;
    snd_pcm_format_t format;
    size_t channels;
    ErlNifBinary src;
    size_t frame_size;
    size_t num_frames;
    size_t sample_size;
    ERL_NIF_TERM list = enif_make_list(env, 0);
    ERL_NIF_TERM dst_bin[MAX_CHANNELS];
    float* dst[MAX_CHANNELS];
    int i, offset;
    
    if (!enif_get_resource(env, argv[0], fft_r, (void **)&fp))
	return enif_make_badarg(env);
    if (!get_format(env, argv[1], &format))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[2], &channels))
	return enif_make_badarg(env);
    if ((channels < 1) || (channels > MAX_CHANNELS))
	return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[3], &src))
	return enif_make_badarg(env);

    sample_size = snd_pcm_format_size(format, 1);
    frame_size = channels*sample_size;
    num_frames = src.size / frame_size;

    // complex outputs 2*float per sample
    for (i = 0; i < (int)channels; i++) {
	dst[i] = (float*) enif_make_new_binary(env,
					       sizeof(float)*2*fp->n,
					       &dst_bin[i]);
    }

    for (i = 0, offset=0; i < (int) channels; i++, offset+=sample_size) {
	size_t n = (num_frames > fp->n) ? fp->n : num_frames;
	int8_t* srcp = (int8_t*) src.data;
	float* ptr = dst[i];
	int j;

	for (j = 0; j < n; j++) {
	    double y = read_pcm_float(format, srcp+offset);
	    ptr[0] = y;
	    ptr[1] = 0;
	    ptr += 2;
	    srcp += frame_size;
	}
	for (; j < fp->n; j++) {
	    ptr[0] = 0.0;
	    ptr[1] = 0.0;
	    ptr += 2;
	}
    }
    for (i = 0; i < (int) channels; i++) {
	fft_fft(fp, (float complex*) dst[i], (float complex*) dst[i]);
    }

    for (i = channels-1; i >= 0; i--) {
	list = enif_make_list_cell(env, dst_bin[i], list);
    }    
    return list;
}

// rfft(FFT, Format, Channels, Data) -> [F1,..Fn]
// output is native float
static ERL_NIF_TERM nif_rfft(ErlNifEnv* env, int argc,
			    const ERL_NIF_TERM argv[])
{
    fft_t* fp;
    snd_pcm_format_t format;
    size_t channels;
    ErlNifBinary src;
    size_t frame_size;
    size_t num_frames;
    size_t sample_size;
    ERL_NIF_TERM list = enif_make_list(env, 0);
    ERL_NIF_TERM dst_bin[MAX_CHANNELS];
    float* dst[MAX_CHANNELS];
    int i, offset;
    
    if (!enif_get_resource(env, argv[0], fft_r, (void **)&fp))
	return enif_make_badarg(env);
    if (!get_format(env, argv[1], &format))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[2], &channels))
	return enif_make_badarg(env);
    if ((channels < 1) || (channels > MAX_CHANNELS))
	return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[3], &src))
	return enif_make_badarg(env);

    sample_size = snd_pcm_format_size(format, 1);
    frame_size = channels*sample_size;
    num_frames = src.size / frame_size;

    // float output
    for (i = 0; i < (int)channels; i++) {
	dst[i] = (float*) enif_make_new_binary(env,
					       sizeof(float)*fp->n,
					       &dst_bin[i]);
    }

    for (i = 0, offset=0; i < (int) channels; i++, offset+=sample_size) {
	size_t n = (num_frames > fp->n) ? fp->n : num_frames;
	int8_t* srcp = (int8_t*) src.data;
	float* ptr = dst[i];
	int j;

	for (j = 0; j < n; j++) {
	    double y = read_pcm_float(format, srcp+offset);
	    ptr[0] = y;
	    ptr += 1;
	    srcp += frame_size;
	}
	for (; j < fp->n; j++) {
	    ptr[0] = 0.0;
	    ptr += 1;
	}
    }
    for (i = 0; i < (int) channels; i++) {
	fft_rfft(fp, dst[i], dst[i]);
    }

    for (i = channels-1; i >= 0; i--) {
	list = enif_make_list_cell(env, dst_bin[i], list);
    }    
    return list;
}

// ifft(FFT, Format, FFTData) -> binary()
// output if complex (according to format)
static ERL_NIF_TERM nif_ifft(ErlNifEnv* env, int argc,
			    const ERL_NIF_TERM argv[])
{
    fft_t* fp;
    snd_pcm_format_t format;
    ErlNifBinary src;
    size_t frame_size;
    size_t num_frames;
    size_t sample_size;
    ERL_NIF_TERM dst_bin;
    int8_t* dst;
    float complex* tmp;
    float* srcp;
    int i;
    
    if (!enif_get_resource(env, argv[0], fft_r, (void **)&fp))
	return enif_make_badarg(env);
    if (!get_format(env, argv[1], &format))
	return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[2], &src))
	return enif_make_badarg(env);

    sample_size = snd_pcm_format_size(format, 1);
    frame_size = 2*sample_size;
    num_frames = src.size / frame_size;
    // must be complex output (normally from fft)
    if (num_frames != fp->n)
	return enif_make_badarg(env);
    if (src.size != num_frames*sizeof(float complex))
	return enif_make_badarg(env);

    srcp = (float*) src.data;
    // copy data
    tmp = (float complex*) malloc(sizeof(float complex)*fp->n);
    for (i = 0; i < fp->n; i++) {
	tmp[i] = CMPLXF(srcp[0], srcp[1]);
	srcp += 2;
    }
    dst = (int8_t*) enif_make_new_binary(env,frame_size*fp->n,&dst_bin);
    fft_ifft(fp, tmp);
    
    for (i = 0; i < fp->n; i++) {
	write_pcm_float(format, creal(tmp[i]), dst);
	dst += sample_size;
	write_pcm_float(format, cimag(tmp[i]), dst);
	dst += sample_size;
    }
    free(tmp);
    return dst_bin;
}


// create all tracing NIFs
#ifdef NIF_TRACE

#undef NIF

static void trace_print_arg_list(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    enif_fprintf(stdout, "(");
    if (argc > 0) {
	int i;
	if (enif_is_ref(env, argv[0])) {
	    // FIXME print object type if available
	    enif_fprintf(stdout, "%T", argv[0]);
	}
	else
	    enif_fprintf(stdout, "%T", argv[0]);
	for (i = 1; i < argc; i++)
	    enif_fprintf(stdout, ",%T", argv[i]);
    }
    enif_fprintf(stdout, ")");
}

#define NIF(name, arity, func)					\
static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]) \
{ \
    ERL_NIF_TERM result;					\
    enif_fprintf(stdout, "ENTER %s", (name));			\
    trace_print_arg_list(env, argc, argv);			\
    enif_fprintf(stdout, "\r\n");				\
    result = func(env, argc, argv);				\
    enif_fprintf(stdout, "  RESULT=%T\r\n", (result));		\
    enif_fprintf(stdout, "LEAVE %s\r\n", (name));		\
    return result;						\
}

NIF_LIST

#endif

static int load_atoms(ErlNifEnv* env)
{
    LOAD_ATOM(ok);
    LOAD_ATOM(error);
    LOAD_ATOM(undefined);
    LOAD_ATOM(enoent);    
    LOAD_ATOM(true);
    LOAD_ATOM(false);
    
    // format
#undef SND_FORMAT
#define SND_FORMAT(atm, form) LOAD_ATOM(atm);
SND_FORMAT_LIST
    /*
    LOAD_ATOM(s8);
    LOAD_ATOM(u8);
    LOAD_ATOM(s16_le);
    LOAD_ATOM(s16_be);
    LOAD_ATOM(u16_le);
    LOAD_ATOM(u16_be);
    LOAD_ATOM(s24_le);
    LOAD_ATOM(s24_be);
    LOAD_ATOM(u24_le);
    LOAD_ATOM(u24_be);
    LOAD_ATOM(s32_le);
    LOAD_ATOM(s32_be);
    LOAD_ATOM(u32_le);
    LOAD_ATOM(u32_be);
    LOAD_ATOM(float_le);
    LOAD_ATOM(float_be);
    LOAD_ATOM(float64_le);
    LOAD_ATOM(float64_be);
    LOAD_ATOM(iec958_subframe_le);
    LOAD_ATOM(iec958_subframe_be);
    LOAD_ATOM(mu_law);
    LOAD_ATOM(a_law);
    LOAD_ATOM(ima_adpcm);
    LOAD_ATOM(g723_24);
    LOAD_ATOM(g723_40);
    LOAD_ATOM(dsd_u8);
    LOAD_ATOM(dsd_u16_le);
    LOAD_ATOM(dsd_u32_le);
    LOAD_ATOM(dsd_u16_be);
    LOAD_ATOM(dsd_u32_be);
    LOAD_ATOM(mpeg);
    LOAD_ATOM(gsm);
    LOAD_ATOM(s20_le);
    LOAD_ATOM(s20_be);
    LOAD_ATOM(u20_le);
    LOAD_ATOM(u20_be);
    LOAD_ATOM(special);
    LOAD_ATOM(s24_3le);
    LOAD_ATOM(s24_3be);
    LOAD_ATOM(u24_3le);
    LOAD_ATOM(u24_3be);
    LOAD_ATOM(s20_3le);
    LOAD_ATOM(s20_3be);
    LOAD_ATOM(u20_3le);
    LOAD_ATOM(u20_3be);
    LOAD_ATOM(s18_3le);
    LOAD_ATOM(s18_3be);
    LOAD_ATOM(u18_3le);
    LOAD_ATOM(u18_3be);
    LOAD_ATOM(g723_24_1b);
    LOAD_ATOM(g723_40_1b);
    */
    return 0;
}

void wave_dtor(ErlNifEnv *env, void *obj)
{
    fft_t *fp = (fft_t *)obj;
    fft_clear(fp);
    DEBUGF("dtor fft%s", "");
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    nif_ctx_t* ctx;
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit cb;     
    
    DEBUGF("load%s", "");

    cb.dtor = (ErlNifResourceDtor*) wave_dtor;
    cb.stop = (ErlNifResourceStop*) NULL;
    cb.down = (ErlNifResourceDown*) NULL;
    
    if ((fft_r =
         enif_open_resource_type_x(env, "fft", &cb,
				   ERL_NIF_RT_CREATE,
				   &tried)) == NULL) {
        return -1;
    }
    if ((ctx = (nif_ctx_t*) enif_alloc(sizeof(nif_ctx_t))) == NULL)
	return -1;
    if (load_atoms(env) < 0)
	return -1;
    hash_helems("format", ctx->format_hash, FORMAT_HASH_SIZE, format_elems);
    *priv_data = ctx;
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data,
		   void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit cb;    
    nif_ctx_t* ctx = (nif_ctx_t*) *old_priv_data;
    
    DEBUGF("upgrade%s", "");

    cb.dtor = (ErlNifResourceDtor*) wave_dtor;
    cb.stop = (ErlNifResourceStop*) NULL;
    cb.down = (ErlNifResourceDown*) NULL;

    if ((fft_r =
	 enif_open_resource_type_x(env, "fft", &cb,
				   ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
				   &tried)) == NULL) {
	return -1;
    }
    if (load_atoms(env) < 0)
	return -1;
    hash_helems("format", ctx->format_hash, FORMAT_HASH_SIZE, format_elems);    
    *priv_data = *old_priv_data;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);
    UNUSED(priv_data);
    DEBUGF("unload%s", "");
}

ERL_NIF_INIT(alsa_fft, nif_funcs, load, NULL, upgrade, unload)
