//
// SBC codec
//
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sbc/sbc.h>

#include "erl_nif.h"
#include "erl_driver.h"

#define MAX_OUTPUT_SIZE 8192 // 5126 for dual channel 16 blocks, 8 subband 

#define DEBUG
#define NIF_TRACE

#define UNUSED(a) ((void) a)

#ifdef DEBUG
#define DEBUGF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#else
#define DEBUGF(f,a...)
#endif
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
    NIF("new", 1,  nif_new)					  \
    NIF("new_", 2,  nif_new)					  \
    NIF("reinit", 2,  nif_reinit)				  \
    NIF("reinit_", 3,  nif_reinit)				  \
    NIF("decode", 2, nif_decode)				  \
    NIF("encode", 2,  nif_encode)				  \
    NIF("get_frame_length", 1, nif_get_frame_length)		  \
    NIF("get_frame_duration", 1, nif_get_frame_duration)	  \
    NIF("get_codesize", 1, nif_get_codesize)			  \
    NIF("get_info", 1, nif_get_info)				  \
    NIF("finish", 1, nif_finish)                                  \

static ErlNifResourceType* sbc_r;

typedef enum {
    SBC = 0,
    MSBC = 1,
    A2DP = 2
} sbc_type_t; 

typedef struct _handle_t {
    sbc_type_t type;
    int        active;
    sbc_t      sbc;
} handle_t;

DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(undefined);
DECL_ATOM(select);
DECL_ATOM(read);
DECL_ATOM(write);
DECL_ATOM(no_such_handle);
DECL_ATOM(too_short);
DECL_ATOM(sync_byte_incorrect);
DECL_ATOM(bad_crc);
DECL_ATOM(bitpool_value_out_of_bounds);

DECL_ATOM(sbc);
DECL_ATOM(msbc);
DECL_ATOM(a2dp);


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


static void dtor(ErlNifEnv* env, handle_t* hp)
{
    UNUSED(env);
    DEBUGF("dtor: hp=%p, active=%d", hp, hp->active);    
    if (hp->active) {
	hp->active = 0;
	sbc_finish(&hp->sbc);
    }
}

static void down(ErlNifEnv* env, handle_t* hp,
		 const ErlNifPid* pid, const ErlNifMonitor* mon)
{
    UNUSED(env);
    UNUSED(pid);
    UNUSED(mon);
    UNUSED(hp);
    DEBUGF("down: hp=%p", hp);
}

static int get_handle(ErlNifEnv* env, ERL_NIF_TERM arg,handle_t** handle_ptr)
		      
{
    handle_t* hp;

    if (!enif_is_ref(env, arg))
	return -1;  // badarg
    if (!enif_get_resource(env, arg, sbc_r, (void **)&hp))
	return 0;   // no a valid reource handle / remove / closed
    if (!hp->active)
	return 0;
    *handle_ptr = hp;
    return 1;
}

static int get_a2dp_config(ErlNifEnv* env, ERL_NIF_TERM arg, uint8_t* conf)
{
    ErlNifBinary bin;

    if (!enif_inspect_binary(env, arg, &bin) || (bin.size != 4))
	return 0;
    conf[0] = bin.data[0];
    conf[1] = bin.data[1];
    conf[2] = bin.data[2];
    conf[3] = bin.data[3];
    return 1;
}


static ERL_NIF_TERM make_error(ErlNifEnv* env, int err)
{
    DEBUGF("make_error: %d", err);
    return enif_make_tuple2(env,
			    ATOM(error),
			    enif_make_atom(env, erl_errno_id(err)));
}

static ERL_NIF_TERM make_decode_error(ErlNifEnv* env, int err)
{
    ERL_NIF_TERM e;
    DEBUGF("make_decode_error: %d", err);
    switch(err) {
    case -1: e = ATOM(too_short); break;
    case -2: e = ATOM(sync_byte_incorrect); break;
    case -3: e = ATOM(bad_crc); break;
    case -4: e = ATOM(bitpool_value_out_of_bounds); break;
    default: return make_error(env, err);
    }
    return enif_make_tuple2(env, ATOM(error), e);
}

// Value must be return from NIF
static ERL_NIF_TERM make_bad_handle(ErlNifEnv* env) {
    DEBUGF("bad_handle%s", "");
    return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
}

static ERL_NIF_TERM nif_new(ErlNifEnv* env, int argc,
			    const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    ERL_NIF_TERM ht;
    sbc_type_t type;
    unsigned long flags = 0;
    int err;
    uint8_t a2dp_config[4];

    if ((argc == 1) && (argv[0] == ATOM(sbc)))
	type = SBC;
    else if ((argc == 1) && (argv[0] == ATOM(msbc)))
	type = MSBC;
    else if ((argc == 2) && (argv[0] == ATOM(a2dp))) {
	if (!get_a2dp_config(env, argv[1], a2dp_config))
	    return enif_make_badarg(env);
	type = A2DP;
    }
    else
	return enif_make_badarg(env);
    
    if ((hp = enif_alloc_resource(sbc_r, sizeof(handle_t))) == NULL)
	return make_error(env, errno);

    memset(hp, 0, sizeof(handle_t));
    switch(type) {
    case SBC:
	if (sbc_init(&hp->sbc, flags) < 0) goto error;
	break;
    case MSBC:
	if (sbc_init_msbc(&hp->sbc, flags) < 0) goto error;
	break;
    case A2DP:
	if (sbc_init_a2dp(&hp->sbc, flags,
			  (void*) a2dp_config,
			  sizeof(a2dp_config)) < 0) goto error;
	break;
    }
    hp->type = type;
    hp->active = 1;
    ht = enif_make_resource(env, hp);
    enif_release_resource(hp);
    return enif_make_tuple2(env, ATOM(ok), ht);
error:
    err = errno;
    enif_release_resource(hp);
    return make_error(env, err);    
}

static ERL_NIF_TERM nif_reinit(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    sbc_type_t type;
    uint8_t a2dp_config[4];
    unsigned long flags = 0;
    
    switch(get_handle(env, argv[0], &hp)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }

    if ((argc == 2) && (argv[1] == ATOM(sbc)))
	type = SBC;
    else if ((argc == 2) && (argv[1] == ATOM(msbc)))
	type = MSBC;
    else if ((argc == 3) && (argv[1] == ATOM(a2dp))) {
	if (!get_a2dp_config(env, argv[2], a2dp_config))
	    return enif_make_badarg(env);
	type = A2DP;
    }
    else
	return enif_make_badarg(env);

    switch(type) {
    case SBC:
	if (sbc_reinit(&hp->sbc, flags) < 0) goto error;
	break;
    case MSBC:
	if (sbc_reinit_msbc(&hp->sbc, flags) < 0) goto error;
	break;
    case A2DP:
	if (sbc_reinit_a2dp(&hp->sbc, flags,
			    (void*) a2dp_config,
			    sizeof(a2dp_config)) < 0) goto error;
	break;
    }
    hp->type = type;
    return ATOM(ok);
error:
    return make_error(env, errno);    
}

// Encode audio pcm data from binary and return a (reversed) list of frames
// return {ok, ListOfFrames, RestOfBinary}
static ERL_NIF_TERM nif_encode(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;
    ErlNifBinary bin;
    size_t codesize;
    size_t frame_len;    
    char* ptr;
    size_t remain;
    ERL_NIF_TERM frame_list;
    ERL_NIF_TERM rest;
    
    if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
	return enif_make_badarg(env);
    ptr = (char*) bin.data;
    remain = bin.size;
    switch(get_handle(env, argv[0], &hp)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    frame_list = enif_make_list(env, 0);
    codesize = sbc_get_codesize(&hp->sbc);
    frame_len = sbc_get_frame_length(&hp->sbc);
    
    while(remain >= codesize) {
	uint8_t output[frame_len];
	ssize_t n;  // number of bytes sent or error if < 0
	ssize_t written;  // frames written 
	ERL_NIF_TERM out;
	char* optr;
	
	if ((n = sbc_encode(&hp->sbc, ptr, codesize,
			    output, sizeof(output),
			    &written)) < 0) {
	    return make_error(env, -n);
	}
	optr = (char*) enif_make_new_binary(env, n, &out);
	memcpy(optr, output, n);
	frame_list = enif_make_list_cell(env, out, frame_list);
	remain -= n;
    }
    if (remain)
	rest = enif_make_sub_binary(env, argv[1], bin.size-remain, remain);
    else
	enif_make_new_binary(env, 0, &rest);
    return enif_make_tuple3(env, ATOM(ok), frame_list, rest);
}

// decode some number of frames return decoded result as a reverse list
static ERL_NIF_TERM nif_decode(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;
    ErlNifBinary bin;
    size_t codesize;
    // size_t frame_len;
    char* ptr;
    char* optr;
    size_t remain;    
    ERL_NIF_TERM pcm_list;
    ERL_NIF_TERM rest;
    
    if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
	return enif_make_badarg(env);
    ptr = (char*) bin.data;
    remain = bin.size;
    
    switch(get_handle(env, argv[0], &hp)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }

    pcm_list = enif_make_list(env, 0);
    codesize = sbc_get_codesize(&hp->sbc);    
    
    while(remain >= codesize) {
	uint8_t output[MAX_OUTPUT_SIZE];
	ssize_t n;  // number of bytes sent or error if < 0
	size_t written;  // frames written 
	ERL_NIF_TERM out;
	
	if ((n = sbc_decode(&hp->sbc, ptr, codesize,
			    output, sizeof(output),
			    &written)) < 0) {
	    return make_decode_error(env, n);
	}
	optr = (char*) enif_make_new_binary(env, n, &out);
	memcpy(optr, output, n);
	pcm_list = enif_make_list_cell(env, out, pcm_list);
	remain -= codesize;
	ptr += codesize;
    }
    // create binary tail (note that input is an iolist!)
    optr = (char*) enif_make_new_binary(env, remain, &rest);
    memcpy(optr, ptr, remain); 
    return enif_make_tuple3(env, ATOM(ok), pcm_list, rest);

}


static ERL_NIF_TERM nif_get_frame_length(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;
    size_t len;    

    switch(get_handle(env, argv[0], &hp)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    len = sbc_get_frame_length(&hp->sbc);
    return enif_make_tuple2(env, ATOM(ok), enif_make_ulong(env, len));
}

static ERL_NIF_TERM nif_get_frame_duration(ErlNifEnv* env, int argc,
					   const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;
    unsigned int duration;
    switch(get_handle(env, argv[0], &hp)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    duration = sbc_get_frame_duration(&hp->sbc);
    return enif_make_tuple2(env, ATOM(ok), enif_make_uint(env, duration));    
}

static ERL_NIF_TERM nif_get_codesize(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;
    size_t size;    
    switch(get_handle(env, argv[0], &hp)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    size = sbc_get_codesize(&hp->sbc);
    return enif_make_tuple2(env, ATOM(ok), enif_make_ulong(env, size));
}

static ERL_NIF_TERM nif_get_info(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;
    const char* str;
    
    switch(get_handle(env, argv[0], &hp)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    str = sbc_get_implementation_info(&hp->sbc);
    return enif_make_tuple2(env, ATOM(ok),
			    str ? enif_make_string(env, str, ERL_NIF_LATIN1) :
			    ATOM(undefined));
}

// finish(Handle::ref()) -> ok
static ERL_NIF_TERM nif_finish(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;

    switch(get_handle(env, argv[0], &hp)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    sbc_finish(&hp->sbc);
    hp->active = 0;
    return ATOM(ok);
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
    LOAD_ATOM(select);
    LOAD_ATOM(read);
    LOAD_ATOM(write);
    LOAD_ATOM(no_such_handle);

    LOAD_ATOM(too_short);
    LOAD_ATOM(sync_byte_incorrect);
    LOAD_ATOM(bad_crc);
    LOAD_ATOM(bitpool_value_out_of_bounds);    
    
    LOAD_ATOM(sbc);
    LOAD_ATOM(msbc);
    LOAD_ATOM(a2dp);
    return 0;
}


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit cb;
    DEBUGF("load%s", "");

    cb.dtor = (ErlNifResourceDtor*) dtor;
    cb.stop = (ErlNifResourceStop*) NULL;
    cb.down = (ErlNifResourceDown*) down;
    if ((sbc_r =
	 enif_open_resource_type_x(env, "sbc",
				   &cb, ERL_NIF_RT_CREATE,
				   &tried)) == NULL) {
	return -1;
    }
    if (load_atoms(env) < 0)
	return -1;
    *priv_data = 0;
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
    
    DEBUGF("upgrade%s", "");

    cb.dtor = (ErlNifResourceDtor*) dtor;
    cb.stop = (ErlNifResourceStop*) NULL;
    cb.down = (ErlNifResourceDown*) down;

    if ((sbc_r = enif_open_resource_type_x(env, "sbc", &cb,
					   ERL_NIF_RT_CREATE|
					   ERL_NIF_RT_TAKEOVER,
					   &tried)) == NULL) {
	return -1;
    }
    if (load_atoms(env) < 0)
	return -1;    
    *priv_data = *old_priv_data;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);
    UNUSED(priv_data);
    DEBUGF("unload%s", "");
}

ERL_NIF_INIT(alsa_sbc, nif_funcs, load, NULL, upgrade, unload)
