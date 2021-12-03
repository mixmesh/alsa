#include <stdbool.h>
#include <erl_nif.h>
#include <alsa/asoundlib.h>

// #define DEBUGF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#define DEBUGF(f,a...)
#define ERRORF(f,a...) enif_fprintf(stderr, f "\r\n", a)
// #define ERRORF(f,a...)

#define UNUSED(a) ((void) a)

// #define NIF_TRACE

#define ATOM(name) atm_##name

#define DECL_ATOM(name) ERL_NIF_TERM atm_##name = 0

#define LOAD_ATOM(name) \
    do { \
        if (!enif_make_existing_atom(env, #name, &atm_##name, ERL_NIF_LATIN1)) { \
	    ERRORF("alsa_nif: atom [%s] not declared", #name);		\
	    return -1;							\
	}								\
    } while (0)

DECL_ATOM(undefined);
DECL_ATOM(true);
DECL_ATOM(false);
DECL_ATOM(ok);
DECL_ATOM(error);

DECL_ATOM(playback);
DECL_ATOM(capture);

DECL_ATOM(format);
DECL_ATOM(channels);
DECL_ATOM(rate);
DECL_ATOM(period_size);
DECL_ATOM(buffer_size);

DECL_ATOM(start_threshold);

DECL_ATOM(underrun);
DECL_ATOM(overrun);
DECL_ATOM(suspend_event);
DECL_ATOM(would_block);
DECL_ATOM(system_call);

// posix errors
DECL_ATOM(eagain);
DECL_ATOM(eintr);
DECL_ATOM(estrpipe);
DECL_ATOM(epipe);

// errors
DECL_ATOM(no_such_handle);
DECL_ATOM(select_already);
DECL_ATOM(bad_param);

// state
DECL_ATOM(open);
DECL_ATOM(setup);	
DECL_ATOM(prepared);
DECL_ATOM(running);
DECL_ATOM(xrun);
DECL_ATOM(draining);
DECL_ATOM(paused);	
DECL_ATOM(suspended);	
DECL_ATOM(disconnected);
DECL_ATOM(private1);

// format
DECL_ATOM(s8);
DECL_ATOM(u8);
DECL_ATOM(s16_le);
DECL_ATOM(s16_be);
DECL_ATOM(u16_le);
DECL_ATOM(u16_be);
DECL_ATOM(s24_le);
DECL_ATOM(s24_be);
DECL_ATOM(u24_le);
DECL_ATOM(u24_be);
DECL_ATOM(s32_le);
DECL_ATOM(s32_be);
DECL_ATOM(u32_le);
DECL_ATOM(u32_be);
DECL_ATOM(float_le);
DECL_ATOM(float_be);
DECL_ATOM(float64_le);
DECL_ATOM(float64_be);
DECL_ATOM(iec958_subframe_le);
DECL_ATOM(iec958_subframe_be);
DECL_ATOM(mu_law);
DECL_ATOM(a_law);
DECL_ATOM(ima_adpcm);
DECL_ATOM(g723_24);
DECL_ATOM(g723_40);
DECL_ATOM(dsd_u8);
DECL_ATOM(dsd_u16_le);
DECL_ATOM(dsd_u32_le);
DECL_ATOM(dsd_u16_be);
DECL_ATOM(dsd_u32_be);
DECL_ATOM(mpeg);
DECL_ATOM(gsm);
DECL_ATOM(s20_le);
DECL_ATOM(s20_be);
DECL_ATOM(u20_le);
DECL_ATOM(u20_be);
DECL_ATOM(special);
DECL_ATOM(s24_3le);
DECL_ATOM(s24_3be);
DECL_ATOM(u24_3le);
DECL_ATOM(u24_3be);
DECL_ATOM(s20_3le);
DECL_ATOM(s20_3be);
DECL_ATOM(u20_3le);
DECL_ATOM(u20_3be);
DECL_ATOM(s18_3le);
DECL_ATOM(s18_3be);
DECL_ATOM(u18_3le);
DECL_ATOM(u18_3be);
DECL_ATOM(g723_24_1b);
DECL_ATOM(g723_40_1b);

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
    NIF("open_", 2,  nif_open)  \
    NIF("close_", 1, nif_close) \
    NIF("read_", 2,  nif_read) \
    NIF("write_", 2, nif_write) \
    NIF("prepare_",  1, nif_prepare) \
    NIF("start_",  1, nif_start) \
    NIF("drain_", 1, nif_drain) \
    NIF("drop_", 1,  nif_drop) \
    NIF("recover_",  2, nif_recover) \
    NIF("reset_",    1, nif_reset) \
    NIF("pause_",    1, nif_pause) \
    NIF("resume_",   1, nif_resume) \
    NIF("avail_",    1, nif_avail) \
    NIF("state_",    1, nif_state) \
    NIF("strerror",  1, nif_strerror) \
    NIF("get_hw_params_", 1, nif_get_hw_params) \
    NIF("set_hw_params_", 2, nif_set_hw_params) \
    NIF("get_sw_params_", 1, nif_get_sw_params) \
    NIF("set_sw_params_", 2, nif_set_sw_params) \
    NIF("select_",        1, nif_select) \
    NIF("format_is_signed", 1, nif_format_is_signed) \
    NIF("format_is_unsigned", 1, nif_format_is_unsigned) \
    NIF("format_is_little", 1, nif_format_is_little) \
    NIF("format_is_big", 1, nif_format_is_big) \
    NIF("format_width", 1, nif_format_width) \
    NIF("format_physical_width", 1, nif_format_physical_width) \
    NIF("format_size", 2, nif_format_size) \
    NIF("format_silence", 1, nif_format_silence) \
    NIF("make_silence", 3, nif_make_silence)

#define MAX_NFDS 4  // 4 fd per pcm_handle?

ErlNifResourceType *handle_resource_type;

typedef struct {
    ErlNifMutex*  access_mtx;
    int           access_count;
    int           is_open;
    snd_pcm_t*    pcm_handle;
    int           nfds;           // number of fds used in select
    struct pollfd fds[MAX_NFDS];  // >=0 when in select
} handle_t;

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

ErlNifFunc nif_funcs[] =
{
    NIF_LIST
};



// Get handle and update access count under lock so pcm_handle
// is not closed while we execute alsa call, must call done_handle!
static int get_handle(ErlNifEnv* env, ERL_NIF_TERM arg,
		      handle_t** handle_ptr) {
    handle_t* handle;
    int r;

    if (!enif_is_ref(env, arg))
	return -1;  // badarg
    if (!enif_get_resource(env, arg, handle_resource_type,(void **)&handle))
	return 0;   // no a valid reource handle / remove / closed
    enif_mutex_lock(handle->access_mtx);
    if ((r = handle->is_open))
	handle->access_count++;
    enif_mutex_unlock(handle->access_mtx);
    *handle_ptr = handle;
    return r;
}

// code calling get_handle must call done_handle at the end of code
static void done_handle(ErlNifEnv* env, handle_t* handle) {
    int must_close = 0;
    DEBUGF("done_handle: access_count = %d", handle->access_count);    
    enif_mutex_lock(handle->access_mtx);
    handle->access_count--;
    if ((handle->access_count == 0) && !handle->is_open)
	must_close = 1;
    enif_mutex_unlock(handle->access_mtx);    
    if (must_close) {
	DEBUGF("done_handle: close", 0);
	if (handle->nfds > 0) {
	    int i;
	    for (i = 0; i < handle->nfds; i++) {
		DEBUGF("done_handle: select STOP fd=%d", handle->fds[i].fd);
		enif_select(env, (ErlNifEvent)handle->fds[i].fd,
			    ERL_NIF_SELECT_STOP,
			    handle, NULL, ATOM(undefined));
	    }
	}
	else {
	    snd_pcm_close(handle->pcm_handle);
	}
    }
}

static ERL_NIF_TERM make_boolean(ErlNifEnv* env, int x)
{
    return x ? ATOM(true) : ATOM(false);
}

// Value must be return from NIF
static ERL_NIF_TERM make_ok(ErlNifEnv* env, handle_t* handle, ERL_NIF_TERM value) {
    done_handle(env, handle);
    return enif_make_tuple2(env, ATOM(ok), value);
}

// Value must be return from NIF
static ERL_NIF_TERM make_ok1(ErlNifEnv* env, handle_t* handle) {
    UNUSED(env);
    done_handle(env, handle);
    return ATOM(ok);
}

// Value must be return from NIF
static ERL_NIF_TERM make_error(ErlNifEnv* env, ERL_NIF_TERM err) {
    DEBUGF("make_error: %T", err);
    return enif_make_tuple2(env, ATOM(error), err);
}

// Value must be return from NIF
static ERL_NIF_TERM make_herror(ErlNifEnv* env, handle_t* handle, ERL_NIF_TERM err) {
    DEBUGF("make_herror: %T", err);
    done_handle(env, handle);
    return enif_make_tuple2(env, ATOM(error), err);
}

// Value must be return from NIF
static ERL_NIF_TERM make_bad_handle(ErlNifEnv* env) {
    DEBUGF("bad_handle%s", "");
    return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
}

static ERL_NIF_TERM make_format(ErlNifEnv* env, snd_pcm_format_t format)
{
    switch(format) {
    case SND_PCM_FORMAT_S8: return ATOM(s8);
    case SND_PCM_FORMAT_U8: return ATOM(u8);
    case SND_PCM_FORMAT_S16_LE: return ATOM(s16_le);
    case SND_PCM_FORMAT_S16_BE: return ATOM(s16_be);
    case SND_PCM_FORMAT_U16_LE: return ATOM(u16_le);
    case SND_PCM_FORMAT_U16_BE: return ATOM(u16_be);
    case SND_PCM_FORMAT_S24_LE: return ATOM(s24_le);
    case SND_PCM_FORMAT_S24_BE: return ATOM(s24_be);
    case SND_PCM_FORMAT_U24_LE: return ATOM(u24_le);
    case SND_PCM_FORMAT_U24_BE: return ATOM(u24_be);
    case SND_PCM_FORMAT_S32_LE: return ATOM(s32_le);
    case SND_PCM_FORMAT_S32_BE: return ATOM(s32_be);
    case SND_PCM_FORMAT_U32_LE: return ATOM(u32_le);
    case SND_PCM_FORMAT_U32_BE: return ATOM(u32_be);
    case SND_PCM_FORMAT_FLOAT_LE: return ATOM(float_le);
    case SND_PCM_FORMAT_FLOAT_BE: return ATOM(float_be);
    case SND_PCM_FORMAT_FLOAT64_LE: return ATOM(float64_le);
    case SND_PCM_FORMAT_FLOAT64_BE: return ATOM(float64_be);
    case SND_PCM_FORMAT_IEC958_SUBFRAME_LE: return ATOM(iec958_subframe_le);
    case SND_PCM_FORMAT_IEC958_SUBFRAME_BE: return ATOM(iec958_subframe_be);
    case SND_PCM_FORMAT_MU_LAW: return ATOM(mu_law);
    case SND_PCM_FORMAT_A_LAW: return ATOM(a_law);
    case SND_PCM_FORMAT_IMA_ADPCM: return ATOM(ima_adpcm);
    case SND_PCM_FORMAT_MPEG: return ATOM(mpeg);
    case SND_PCM_FORMAT_GSM: return ATOM(gsm);
    case SND_PCM_FORMAT_S20_LE: return ATOM(s20_le);
    case SND_PCM_FORMAT_S20_BE: return ATOM(s20_be);
    case SND_PCM_FORMAT_U20_LE: return ATOM(u20_le);
    case SND_PCM_FORMAT_U20_BE: return ATOM(u20_be);
    case SND_PCM_FORMAT_SPECIAL: return ATOM(special);
    case SND_PCM_FORMAT_S24_3LE: return ATOM(s24_3le);
    case SND_PCM_FORMAT_S24_3BE: return ATOM(s24_3be);
    case SND_PCM_FORMAT_U24_3LE: return ATOM(u24_3le);
    case SND_PCM_FORMAT_U24_3BE: return ATOM(u24_3be);
    case SND_PCM_FORMAT_S20_3LE: return ATOM(s20_3le);
    case SND_PCM_FORMAT_S20_3BE: return ATOM(s20_3be);
    case SND_PCM_FORMAT_U20_3LE: return ATOM(u20_3le);
    case SND_PCM_FORMAT_U20_3BE: return ATOM(u20_3be);
    case SND_PCM_FORMAT_S18_3LE: return ATOM(s18_3le);
    case SND_PCM_FORMAT_S18_3BE: return ATOM(s18_3be);
    case SND_PCM_FORMAT_U18_3LE: return ATOM(u18_3le);
    case SND_PCM_FORMAT_U18_3BE: return ATOM(u18_3be);
    case SND_PCM_FORMAT_G723_24: return ATOM(g723_24);
    case SND_PCM_FORMAT_G723_24_1B: return ATOM(g723_24_1b);
    case SND_PCM_FORMAT_G723_40: return ATOM(g723_40);
    case SND_PCM_FORMAT_G723_40_1B: return ATOM(g723_40_1b);
    case SND_PCM_FORMAT_DSD_U8: return ATOM(dsd_u8);
    case SND_PCM_FORMAT_DSD_U16_LE: return ATOM(dsd_u16_le);
    case SND_PCM_FORMAT_DSD_U32_LE: return ATOM(dsd_u32_le);
    case SND_PCM_FORMAT_DSD_U16_BE: return ATOM(dsd_u16_be);
    case SND_PCM_FORMAT_DSD_U32_BE: return ATOM(dsd_u32_be);
    default: return ATOM(undefined);
    }
}

// FIXME: speed up with little hash table (in ctx) if needed
static int get_format(ErlNifEnv* env, ERL_NIF_TERM arg,
		      snd_pcm_format_t* format_ptr)
{
    if (enif_is_atom(env, arg)) {
	if (arg == ATOM(s8)) *format_ptr = SND_PCM_FORMAT_S8;
	else if (arg == ATOM(s8)) *format_ptr = SND_PCM_FORMAT_S8;
	else if (arg == ATOM(u8)) *format_ptr = SND_PCM_FORMAT_U8;
	else if (arg == ATOM(s16_le)) *format_ptr = SND_PCM_FORMAT_S16_LE;
	else if (arg == ATOM(s16_be)) *format_ptr = SND_PCM_FORMAT_S16_BE;
	else if (arg == ATOM(u16_le)) *format_ptr = SND_PCM_FORMAT_U16_LE;
	else if (arg == ATOM(u16_be)) *format_ptr = SND_PCM_FORMAT_U16_BE;
	else if (arg == ATOM(s24_le)) *format_ptr = SND_PCM_FORMAT_S24_LE;
	else if (arg == ATOM(s24_be)) *format_ptr = SND_PCM_FORMAT_S24_BE;
	else if (arg == ATOM(u24_le)) *format_ptr = SND_PCM_FORMAT_U24_LE;
	else if (arg == ATOM(u24_be)) *format_ptr = SND_PCM_FORMAT_U24_BE;
	else if (arg == ATOM(s32_le)) *format_ptr = SND_PCM_FORMAT_S32_LE;
	else if (arg == ATOM(s32_be)) *format_ptr = SND_PCM_FORMAT_S32_BE;
	else if (arg == ATOM(u32_le)) *format_ptr = SND_PCM_FORMAT_U32_LE;
	else if (arg == ATOM(u32_be)) *format_ptr = SND_PCM_FORMAT_U32_BE;
	else if (arg == ATOM(float_le)) *format_ptr = SND_PCM_FORMAT_FLOAT_LE;
	else if (arg == ATOM(float_be)) *format_ptr = SND_PCM_FORMAT_FLOAT_BE;
	else if (arg == ATOM(float64_le)) *format_ptr = SND_PCM_FORMAT_FLOAT64_LE;
	else if (arg == ATOM(float64_be)) *format_ptr = SND_PCM_FORMAT_FLOAT64_BE;
	else if (arg == ATOM(iec958_subframe_le)) *format_ptr = SND_PCM_FORMAT_IEC958_SUBFRAME_LE;
	else if (arg == ATOM(iec958_subframe_be)) *format_ptr = SND_PCM_FORMAT_IEC958_SUBFRAME_BE;
	else if (arg == ATOM(mu_law)) *format_ptr = SND_PCM_FORMAT_MU_LAW;
	else if (arg == ATOM(a_law)) *format_ptr = SND_PCM_FORMAT_A_LAW;
	else if (arg == ATOM(ima_adpcm)) *format_ptr = SND_PCM_FORMAT_IMA_ADPCM;
	else if (arg == ATOM(mpeg)) *format_ptr = SND_PCM_FORMAT_MPEG;
	else if (arg == ATOM(gsm)) *format_ptr = SND_PCM_FORMAT_GSM;
	else if (arg == ATOM(s20_le)) *format_ptr = SND_PCM_FORMAT_S20_LE;
	else if (arg == ATOM(s20_be)) *format_ptr = SND_PCM_FORMAT_S20_BE;
	else if (arg == ATOM(u20_le)) *format_ptr = SND_PCM_FORMAT_U20_LE;
	else if (arg == ATOM(u20_be)) *format_ptr = SND_PCM_FORMAT_U20_BE;
	else if (arg == ATOM(special)) *format_ptr = SND_PCM_FORMAT_SPECIAL;
	else if (arg == ATOM(s24_3le)) *format_ptr = SND_PCM_FORMAT_S24_3LE;
	else if (arg == ATOM(s24_3be)) *format_ptr = SND_PCM_FORMAT_S24_3BE;
	else if (arg == ATOM(u24_3le)) *format_ptr = SND_PCM_FORMAT_U24_3LE;
	else if (arg == ATOM(u24_3be)) *format_ptr = SND_PCM_FORMAT_U24_3BE;
	else if (arg == ATOM(s20_3le)) *format_ptr = SND_PCM_FORMAT_S20_3LE;
	else if (arg == ATOM(s20_3be)) *format_ptr = SND_PCM_FORMAT_S20_3BE;
	else if (arg == ATOM(u20_3le)) *format_ptr = SND_PCM_FORMAT_U20_3LE;
	else if (arg == ATOM(u20_3be)) *format_ptr = SND_PCM_FORMAT_U20_3BE;
	else if (arg == ATOM(s18_3le)) *format_ptr = SND_PCM_FORMAT_S18_3LE;
	else if (arg == ATOM(s18_3be)) *format_ptr = SND_PCM_FORMAT_S18_3BE;
	else if (arg == ATOM(u18_3le)) *format_ptr = SND_PCM_FORMAT_U18_3LE;
	else if (arg == ATOM(u18_3be)) *format_ptr = SND_PCM_FORMAT_U18_3BE;
	else if (arg == ATOM(g723_24)) *format_ptr = SND_PCM_FORMAT_G723_24;
	else if (arg == ATOM(g723_24_1b)) *format_ptr = SND_PCM_FORMAT_G723_24_1B;
	else if (arg == ATOM(g723_40)) *format_ptr = SND_PCM_FORMAT_G723_40;
	else if (arg == ATOM(g723_40_1b)) *format_ptr = SND_PCM_FORMAT_G723_40_1B;
	else if (arg == ATOM(dsd_u8)) *format_ptr = SND_PCM_FORMAT_DSD_U8;
	else if (arg == ATOM(dsd_u16_le)) *format_ptr = SND_PCM_FORMAT_DSD_U16_LE;
	else if (arg == ATOM(dsd_u32_le)) *format_ptr = SND_PCM_FORMAT_DSD_U32_LE;
	else if (arg == ATOM(dsd_u16_be)) *format_ptr = SND_PCM_FORMAT_DSD_U16_BE;
	else if (arg == ATOM(dsd_u32_be)) *format_ptr = SND_PCM_FORMAT_DSD_U32_BE;
	else return 0;
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


ERL_NIF_TERM get_hw_params_map(ErlNifEnv* env, snd_pcm_t *pcm_handle,
                               ERL_NIF_TERM *hw_params_map) {
    int err;

    snd_pcm_hw_params_t *hw_params;
    if ((err = snd_pcm_hw_params_malloc(&hw_params)) < 0) {
        return enif_make_int(env, err);
    }

    if ((err = snd_pcm_hw_params_current(pcm_handle, hw_params)) < 0) {
        snd_pcm_hw_params_free(hw_params);
        return enif_make_int(env, err);
    }

    snd_pcm_format_t format;
    snd_pcm_hw_params_get_format(hw_params, &format);
    unsigned int channels;
    snd_pcm_hw_params_get_channels(hw_params, &channels);
    unsigned int rate;
    int dir;
    snd_pcm_hw_params_get_rate(hw_params, &rate, &dir);
    snd_pcm_uframes_t period_size;
    snd_pcm_hw_params_get_period_size(hw_params, &period_size, &dir);
    snd_pcm_uframes_t buffer_size;
    snd_pcm_hw_params_get_buffer_size(hw_params, &buffer_size);

    ERL_NIF_TERM hw_params_keys[5] =
        {
         ATOM(format),
         ATOM(channels),
         ATOM(rate),
         ATOM(period_size),
         ATOM(buffer_size)
        };

    ERL_NIF_TERM hw_params_values[5] =
        {
	 make_format(env, format),
         enif_make_uint(env, channels),
         enif_make_uint(env, rate),
         enif_make_uint(env, period_size),
         enif_make_uint(env, buffer_size)
        };

    enif_make_map_from_arrays(env, hw_params_keys, hw_params_values, 5,
                              hw_params_map);

    return 0;
}

ERL_NIF_TERM set_hw_params_map(ErlNifEnv* env, snd_pcm_t *pcm_handle,
                               ERL_NIF_TERM hw_params_map) {
    int err;

    snd_pcm_hw_params_t *hw_params;
    if ((err = snd_pcm_hw_params_malloc(&hw_params)) < 0) {
        return enif_make_int(env, err);
    }

    if ((err = snd_pcm_hw_params_any(pcm_handle, hw_params)) < 0) {
        snd_pcm_hw_params_free(hw_params);
        return enif_make_int(env, err);
    }

    if ((err = snd_pcm_hw_params_set_access(
                   pcm_handle, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED)) <
        0) {
        snd_pcm_hw_params_free(hw_params);
        return enif_make_int(env, err);
    }

    size_t size;
    if (enif_get_map_size(env, hw_params_map, &size)) {
        if (size > 0) {
            ErlNifMapIterator iter;
            if (enif_map_iterator_create(env, hw_params_map, &iter,
                                         ERL_NIF_MAP_ITERATOR_FIRST)) {
                ERL_NIF_TERM key, value, reason = 0;
                bool badarg = false;
                while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
                    if (key == ATOM(format)) {
                        snd_pcm_format_t format;
                        if (get_format(env, value, &format)) {
			    DEBUGF("format=%d", format);
                            if ((err = snd_pcm_hw_params_set_format(
                                           pcm_handle, hw_params,
                                           format)) < 0) {
                                reason = enif_make_tuple4(
                                             env, ATOM(bad_param), ATOM(format),
                                             value, enif_make_int(env, err));
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else if (key == ATOM(channels)) {
                        unsigned int channels;
                        if (enif_get_uint(env, value, &channels)) {
			    DEBUGF("channels=%d", channels);
                            if ((err = snd_pcm_hw_params_set_channels(
                                           pcm_handle, hw_params,
                                           channels)) < 0) {
                                reason = enif_make_tuple4(
                                             env, ATOM(bad_param),
                                             ATOM(channels), value,
                                             enif_make_int(env, err));
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else if (key == ATOM(rate)) {
                        unsigned int rate;
                        int dir;
                        if (enif_get_uint(env, value, &rate)) {
			    DEBUGF("rate=%u", rate);
                            if ((err = snd_pcm_hw_params_set_rate_near(
                                           pcm_handle, hw_params, &rate, &dir)) < 0) {
                                reason = enif_make_tuple4(
                                             env, ATOM(bad_param), ATOM(rate),
                                             value, enif_make_int(env, err));
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else if (key == ATOM(period_size)) {
                        snd_pcm_uframes_t period_size;
                        int dir;
                        if (enif_get_ulong(env, value, &period_size)) {
			    DEBUGF("period_size=%lu", period_size);
                            if ((err = snd_pcm_hw_params_set_period_size_near(
                                           pcm_handle, hw_params,
                                           &period_size, &dir)) < 0) {
                                reason = enif_make_tuple4(
                                            env, ATOM(bad_param),
                                            ATOM(period_size), value,
                                            enif_make_int(env, err));
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else if (key == ATOM(buffer_size)) {
                        snd_pcm_uframes_t buffer_size;
                        if (enif_get_ulong(env, value, &buffer_size)) {
			    DEBUGF("buffer_size=%lu", buffer_size);
                            if ((err = snd_pcm_hw_params_set_buffer_size_near(
                                           pcm_handle, hw_params,
                                           &buffer_size)) < 0) {
                                reason = enif_make_tuple4(
                                             env, ATOM(bad_param),
                                             ATOM(buffer_size), value,
                                             enif_make_int(env, err));
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else {
                        badarg = true;
                        break;
                    }
                    enif_map_iterator_next(env, &iter);
                }
                enif_map_iterator_destroy(env, &iter);

                if (badarg) {
                    snd_pcm_hw_params_free(hw_params);
                    return enif_make_badarg(env);
                } else if (reason != 0) {
                    snd_pcm_hw_params_free(hw_params);
                    return reason;
                } else {
                    if ((err = snd_pcm_hw_params(
                                   pcm_handle, hw_params)) < 0) {
                        snd_pcm_hw_params_free(hw_params);
                        return enif_make_int(env, err);
                    }

                    snd_pcm_hw_params_free(hw_params);
                    return 0;
                }
            } else {
                snd_pcm_hw_params_free(hw_params);
                return enif_make_badarg(env);
            }
        }
    } else {
        snd_pcm_hw_params_free(hw_params);
        return enif_make_badarg(env);
    }

    return 0;
}

ERL_NIF_TERM get_sw_params_map(ErlNifEnv* env, snd_pcm_t *pcm_handle,
                               ERL_NIF_TERM *sw_params_map) {
    int err;

    snd_pcm_sw_params_t *sw_params;
    if ((err = snd_pcm_sw_params_malloc(&sw_params)) < 0) {
        return enif_make_int(env, err);
    }

    if ((err = snd_pcm_sw_params_current(pcm_handle, sw_params)) < 0) {
        snd_pcm_sw_params_free(sw_params);
        return enif_make_int(env, err);
    }

    snd_pcm_uframes_t start_threshold;
    snd_pcm_sw_params_get_start_threshold(sw_params, &start_threshold);

    ERL_NIF_TERM sw_params_keys[1] = {ATOM(start_threshold)};
    ERL_NIF_TERM sw_params_values[1] = {enif_make_uint(env, start_threshold)};

    enif_make_map_from_arrays(env, sw_params_keys, sw_params_values, 1,
                              sw_params_map);
    return 0;
}

ERL_NIF_TERM set_sw_params_map(ErlNifEnv* env, snd_pcm_t *pcm_handle,
                               ERL_NIF_TERM sw_params_map) {
    int err;

    snd_pcm_sw_params_t *sw_params;
    if ((err = snd_pcm_sw_params_malloc(&sw_params)) < 0) {
        return enif_make_int(env, err);
    }

    if ((err = snd_pcm_sw_params_current(pcm_handle, sw_params)) < 0) {
        snd_pcm_sw_params_free(sw_params);
        return enif_make_int(env, err);
    }

    size_t size;
    if (enif_get_map_size(env, sw_params_map, &size)) {
        if (size > 0) {
            ErlNifMapIterator iter;
            if (enif_map_iterator_create(env, sw_params_map, &iter,
                                         ERL_NIF_MAP_ITERATOR_FIRST)) {
                ERL_NIF_TERM key, value, reason = 0;
                bool badarg = false;
                while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
                    if (key == ATOM(start_threshold)) {
                        snd_pcm_uframes_t start_threshold;
                        if (enif_get_ulong(env, value, &start_threshold)) {
                            if ((err = snd_pcm_sw_params_set_start_threshold(
                                           pcm_handle, sw_params,
                                           start_threshold)) < 0) {
                                reason = enif_make_tuple4(
                                             env, ATOM(bad_param),
                                             ATOM(start_threshold), value,
                                             enif_make_int(env, err));
                                break;
                            }
                        } else {
                            badarg = true;
                            break;
                        }
                    } else {
                        badarg = true;
                        break;
                    }
                    enif_map_iterator_next(env, &iter);
                }
                enif_map_iterator_destroy(env, &iter);

                if (badarg) {
                    snd_pcm_sw_params_free(sw_params);
                    return enif_make_badarg(env);
                } else if (reason != 0) {
                    snd_pcm_sw_params_free(sw_params);
                    return reason;
                } else {
                    if ((err = snd_pcm_sw_params(
                                   pcm_handle, sw_params)) < 0) {
                        snd_pcm_sw_params_free(sw_params);
                        return enif_make_int(env, err);
                    }

                    snd_pcm_sw_params_free(sw_params);
                    return 0;
                }
            } else {
                snd_pcm_sw_params_free(sw_params);
                return enif_make_badarg(env);
            }
        }
    } else {
        snd_pcm_sw_params_free(sw_params);
        return enif_make_badarg(env);
    }

    return 0;
}


/*
 * strerror
 */

#define MAX_STRERROR_LEN 256
static char strerror_buf[MAX_STRERROR_LEN];

static ERL_NIF_TERM nif_strerror(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[]) {
    int err, arity;
    const ERL_NIF_TERM *terms;
    if (enif_get_int(env, argv[0], &err)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN, snd_strerror(err));
    }
    else if (argv[0] == ATOM(would_block)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                      "operation is non-blocking");
    }
    else if (argv[0] == ATOM(no_such_handle)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN, "Alsa handle is stale");
    }
    else if (argv[0] == ATOM(underrun)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                      "Failed to recover from underrun");
    }
    else if (argv[0] == ATOM(overrun)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                      "Failed to recover from overrun");
    }
    else if (argv[0] == ATOM(suspend_event)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                      "Failed to recover from suspend event");
    }
    else if (argv[0] == ATOM(eagain)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN,"EAGAIN");
    }
    else if (argv[0] == ATOM(eintr)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN,"EINTR");
    }
    else if (argv[0] == ATOM(epipe)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN,"EPIPE");
    }
    else if (argv[0] == ATOM(estrpipe)) {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN,"ESTRPIPE");
    }
    else if (enif_get_tuple(env, argv[0], &arity, &terms)) {
        if (terms[0] == ATOM(bad_param) && arity == 4) {
            int err;
            enif_get_int(env, terms[3], &err);
            enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                          "%T cannot be %T (%s)", terms[1], terms[2],
                          snd_strerror(err));
        } else {
            enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                          "Unknown error: %T", argv[0]);
        }
    }
    else {
        enif_snprintf(strerror_buf, MAX_STRERROR_LEN,
                      "Unknown error: %T", argv[0]);
    }
    return enif_make_string(env, strerror_buf, ERL_NIF_LATIN1);
}

/*
 * open
 */

#define MAX_PCM_NAME_LEN 64

static ERL_NIF_TERM nif_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int err;

    char pcm_name[MAX_PCM_NAME_LEN];
    if (enif_get_string(env, argv[0], pcm_name, MAX_PCM_NAME_LEN,
                        ERL_NIF_LATIN1) < 1) {
        return enif_make_badarg(env);
    }

    snd_pcm_stream_t stream;
    if (argv[1] == ATOM(playback)) {
        stream = SND_PCM_STREAM_PLAYBACK;
    } else if (argv[1] == ATOM(capture)) {
        stream = SND_PCM_STREAM_CAPTURE;
    } else {
        return enif_make_badarg(env);
    }

    snd_pcm_t *pcm_handle;
    if ((err = snd_pcm_open(&pcm_handle,
			    pcm_name,
			    stream,
			    SND_PCM_NONBLOCK
	     )) < 0) {
        return enif_make_tuple2(env, ATOM(error), enif_make_int(env, err));
    }

    handle_t *handle =
        enif_alloc_resource(handle_resource_type, sizeof(handle_t));
    handle->pcm_handle = pcm_handle;
    handle->access_mtx = enif_mutex_create("pcm_access");
    handle->access_count = 0;
    handle->is_open = 1;
    ERL_NIF_TERM handle_resource = enif_make_resource(env, handle);
    enif_release_resource(handle);

    return enif_make_tuple2(env, ATOM(ok), handle_resource);
}


static ERL_NIF_TERM nif_close(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    
    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }

    enif_mutex_lock(handle->access_mtx);
    handle->is_open = 0;
    enif_mutex_unlock(handle->access_mtx);
    done_handle(env, handle);
    return ATOM(ok);
}

/*
 * get_hw_params
 */

static ERL_NIF_TERM nif_get_hw_params(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[]) {

    handle_t *handle;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    ERL_NIF_TERM hw_params_map, reason;
    if ((reason = get_hw_params_map(env, handle->pcm_handle,
                                    &hw_params_map)) < 0) {
        return make_herror(env, handle, reason);
    }
    return make_ok(env, handle, hw_params_map);
}

/*
 * set_hw_params
 */

static ERL_NIF_TERM nif_set_hw_params(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    handle_t *handle;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    ERL_NIF_TERM reason;
    if ((reason = set_hw_params_map(env, handle->pcm_handle, argv[1])) < 0) {
        return make_herror(env, handle, reason);
    }

    ERL_NIF_TERM hw_params_map;
    if ((reason = get_hw_params_map(env, handle->pcm_handle,
                                    &hw_params_map)) != 0) {
        return make_herror(env, handle, reason);
    }

    return make_ok(env, handle, hw_params_map);
}

/*
 * get_sw_params
 */

static ERL_NIF_TERM nif_get_sw_params(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[]) {
    handle_t *handle;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    ERL_NIF_TERM sw_params_map, reason;
    if ((reason = get_sw_params_map(env, handle->pcm_handle,
                                    &sw_params_map)) != 0) {
        return make_herror(env, handle, reason);
    }

    return make_ok(env, handle, sw_params_map);
}

/*
 * set_sw_params
 */

static ERL_NIF_TERM nif_set_sw_params(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[]) {
    handle_t *handle;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    ERL_NIF_TERM reason;
    if ((reason = set_sw_params_map(env, handle->pcm_handle, argv[1])) < 0) {
        return make_herror(env, handle, reason);
    }

    ERL_NIF_TERM sw_params_map;
    if ((reason = get_sw_params_map(env, handle->pcm_handle,
                                    &sw_params_map)) != 0) {
        return make_herror(env, handle, reason);
    }

    return make_ok(env, handle, sw_params_map);
}

/*
 * read
 */

static ERL_NIF_TERM nif_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    snd_pcm_uframes_t frames;
    
    if (!enif_get_ulong(env, argv[1], &frames)) {
        return enif_make_badarg(env);
    }

    handle_t *handle;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }

    // read only frames ready? is that enough to get alsa ticking?
    ssize_t binlen = snd_pcm_frames_to_bytes(handle->pcm_handle, frames);
    ErlNifBinary bin;
    // FIXME: make binary pool
    if (!enif_alloc_binary(binlen, &bin)) {
	done_handle(env, handle);
        return enif_make_badarg(env);
    }

    snd_pcm_sframes_t read_frames =
        snd_pcm_readi(handle->pcm_handle, bin.data, frames);
    if (read_frames < 0) {
	enif_release_binary(&bin);

	if (read_frames == -EAGAIN)
	    return make_herror(env, handle, ATOM(eagain));
	else if (read_frames == -EPIPE)
	    return make_herror(env, handle, ATOM(epipe));
	else if (read_frames == -ESTRPIPE)
	    return make_herror(env, handle, ATOM(estrpipe));
	return make_herror(env, handle,
			  enif_make_int(env, read_frames));
    }
    ERL_NIF_TERM hdr;
    hdr = enif_make_int(env, read_frames); // add sample type etc here!
    return make_ok(env, handle,
		     enif_make_tuple2(env, hdr, enif_make_binary(env, &bin)));
}

/*
 * write
 */

static ERL_NIF_TERM nif_write(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    ErlNifBinary bin;
    
    if (!enif_inspect_binary(env, argv[1], &bin))
	return enif_make_badarg(env);

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    snd_pcm_uframes_t frames =
	snd_pcm_bytes_to_frames(handle->pcm_handle, bin.size);
    if (frames == 0)
	return make_ok(env, handle, enif_make_int(env, 0));

    snd_pcm_sframes_t written_frames =
	snd_pcm_writei(handle->pcm_handle, bin.data, frames);

    if (written_frames < 0) {
	if (written_frames == -EAGAIN)
	    return make_herror(env, handle, ATOM(eagain));
	if (written_frames == -EINTR)
	    return make_herror(env, handle, ATOM(eintr));
	else if (written_frames == -EPIPE)
	    return make_herror(env, handle, ATOM(epipe));
	else if (written_frames == -ESTRPIPE)
	    return make_herror(env, handle, ATOM(estrpipe));
	return make_herror(env, handle,
			  enif_make_int(env, written_frames));
    }
    ssize_t nbytes =
	snd_pcm_frames_to_bytes(handle->pcm_handle, written_frames);
    return make_ok(env, handle, enif_make_int(env, nbytes));
}

static ERL_NIF_TERM nif_recover(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    int err;
    ERL_NIF_TERM reason = ATOM(undefined);

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }

    if (argv[1] == ATOM(epipe)) {
	err = -EPIPE;
	reason = ATOM(underrun);
    }
    else if (argv[1] == ATOM(estrpipe)) {
	err = -ESTRPIPE;
	reason = ATOM(suspend_event);
    }
    else if (argv[1] == ATOM(eintr)) {
	err = -EINTR;
	reason = ATOM(system_call);
    }
    else
	return enif_make_badarg(env);
    if (snd_pcm_recover(handle->pcm_handle, err, 1) < 0)
	return enif_make_tuple2(env, ATOM(error), reason);
    return make_ok1(env, handle);
}

static ERL_NIF_TERM nif_drain(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    int err;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    if ((err = snd_pcm_drain(handle->pcm_handle)) < 0) {
	if (err == -EAGAIN) {
	    snd_pcm_sframes_t remain = snd_pcm_avail(handle->pcm_handle);
	    if (remain == -EBADFD) // not running?
		return make_ok1(env, handle);
	    else if (remain < 0)
		return make_herror(env, handle, enif_make_int(env, remain));
	    else if (remain == 0)
		return make_ok1(env, handle);
	    return make_herror(env, handle, ATOM(eagain));
	}
        return make_herror(env, handle, enif_make_int(env, err));
    }
    return make_ok1(env, handle);    
}

static ERL_NIF_TERM nif_drop(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    int err;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    if ((err = snd_pcm_drop(handle->pcm_handle)) < 0)
        return make_herror(env, handle, enif_make_int(env, err));
    return make_ok1(env, handle);        
}

static ERL_NIF_TERM nif_prepare(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    int err;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }

    if ((err = snd_pcm_prepare(handle->pcm_handle)) < 0)
        return make_herror(env, handle, enif_make_int(env, err));
    return make_ok1(env, handle);
}


static ERL_NIF_TERM nif_start(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    int err;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    if ((err = snd_pcm_start(handle->pcm_handle)) < 0)
        return make_herror(env, handle, enif_make_int(env, err));
    return make_ok1(env, handle);        
}

static ERL_NIF_TERM nif_reset(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    int err;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }

    if ((err = snd_pcm_reset(handle->pcm_handle)) < 0)
        return make_herror(env, handle, enif_make_int(env, err));
    return make_ok1(env, handle);
}

static ERL_NIF_TERM nif_pause(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    int enable;
    int err;

    if (argv[1] == ATOM(true))
	enable = 1;
    else if (argv[1] == ATOM(false))
	enable = 0;
    else
	return enif_make_badarg(env);

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }

    if ((err = snd_pcm_pause(handle->pcm_handle, enable)) < 0)
        return make_herror(env, handle, enif_make_int(env, err));
    return make_ok1(env, handle);
}

static ERL_NIF_TERM nif_resume(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    int err;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    if ((err = snd_pcm_resume(handle->pcm_handle)) < 0)
        return make_herror(env, handle, enif_make_int(env, err));
    return make_ok1(env, handle);
}


static ERL_NIF_TERM nif_avail(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    snd_pcm_sframes_t frames;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    if ((frames = snd_pcm_avail(handle->pcm_handle)) < 0)
        return make_herror(env, handle, enif_make_long(env, frames));
    return make_ok(env, handle, enif_make_long(env, frames));
}

static ERL_NIF_TERM nif_state(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]) {
    handle_t *handle;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    switch (snd_pcm_state(handle->pcm_handle)) {
    case SND_PCM_STATE_OPEN:
	return make_ok(env, handle, ATOM(open));
    case SND_PCM_STATE_SETUP:
	return make_ok(env, handle, ATOM(setup));	
    case SND_PCM_STATE_PREPARED:
		return make_ok(env, handle, ATOM(prepared));
    case SND_PCM_STATE_RUNNING:
	return make_ok(env, handle, ATOM(running));
    case SND_PCM_STATE_XRUN:
	return make_ok(env, handle, ATOM(xrun));
    case SND_PCM_STATE_DRAINING:
	return make_ok(env, handle, ATOM(draining));
    case SND_PCM_STATE_PAUSED:
	return make_ok(env, handle, ATOM(paused));	
    case SND_PCM_STATE_SUSPENDED:
	return make_ok(env, handle, ATOM(suspended));	
    case SND_PCM_STATE_DISCONNECTED:
	return make_ok(env, handle, ATOM(disconnected));	
    case SND_PCM_STATE_PRIVATE1:
	return make_ok(env, handle, ATOM(private1));
    default:
	return make_ok(env, handle, ATOM(undefined));
    }
}

static ERL_NIF_TERM nif_select(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    int nfds;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }

    // how do we now we are still in select?
    // FIXME: mutex
    // if (handle->nfds > 0)
    //  return make_herror(env, handle, ATOM(select_already));
    nfds = snd_pcm_poll_descriptors_count(handle->pcm_handle);
    if (nfds > MAX_NFDS) {
	ERRORF("poll_descriptor_count > MAX_NFDS (%d>%d)", nfds, MAX_NFDS);
	nfds = MAX_NFDS;
    }
    handle->nfds = nfds;
    if (nfds > 0) {
	enum ErlNifSelectFlags mode = 0;
	ErlNifPid pid;
	int i;

	handle->nfds = nfds;
	snd_pcm_poll_descriptors(handle->pcm_handle, handle->fds, nfds);

	enif_self(env, &pid);
	for (i = 0; i < nfds; i++) {
	    if (handle->fds[i].events & POLLIN)
		mode |= ERL_NIF_SELECT_READ;
	    if (handle->fds[i].events & POLLOUT)
		mode |= ERL_NIF_SELECT_WRITE;
	    enif_select(env, handle->fds[i].fd, mode, handle,
			&pid, ATOM(undefined));
	}
    }
    return make_ok1(env, handle);
}


static ERL_NIF_TERM nif_format_is_signed(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[]) {
    UNUSED(argc);
    snd_pcm_format_t format;
    int r;
    if (!get_format(env, argv[0], &format))
	return enif_make_badarg(env);
    if ((r = snd_pcm_format_signed(format)) < 0)
	return make_error(env, enif_make_int(env, r));
    return make_boolean(env, r);
}

static ERL_NIF_TERM nif_format_is_unsigned(ErlNifEnv* env, int argc,
					   const ERL_NIF_TERM argv[]) {
    UNUSED(argc);
    snd_pcm_format_t format;
    int r;
    if (!get_format(env, argv[0], &format))
	return enif_make_badarg(env);
    if ((r = snd_pcm_format_unsigned(format)) < 0)
	return make_error(env, enif_make_int(env, r));
    return make_boolean(env, r);
}

static ERL_NIF_TERM nif_format_is_little(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[]) {
    UNUSED(argc);
    snd_pcm_format_t format;
    int r;    
    if (!get_format(env, argv[0], &format))
	return enif_make_badarg(env);
    if ((r = snd_pcm_format_little_endian(format)) < 0)
	return make_error(env, enif_make_int(env, r));
    return make_boolean(env, r);    
}

static ERL_NIF_TERM nif_format_is_big(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[]) {
    UNUSED(argc);
    snd_pcm_format_t format;
    int r;
    if (!get_format(env, argv[0], &format))
	return enif_make_badarg(env);
    if ((r = snd_pcm_format_big_endian(format)) < 0)
	return make_error(env, enif_make_int(env, r));
    return make_boolean(env, r);    
}

static ERL_NIF_TERM nif_format_width(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[]) {
    UNUSED(argc);
    snd_pcm_format_t format;
    int r;
    if (!get_format(env, argv[0], &format))
	return enif_make_badarg(env);
    if ((r = snd_pcm_format_width(format)) < 0)
	return make_error(env, enif_make_int(env, r));
    return enif_make_int(env, r);
}

static ERL_NIF_TERM nif_format_physical_width(ErlNifEnv* env, int argc,
					      const ERL_NIF_TERM argv[]) {
    UNUSED(argc);
    snd_pcm_format_t format;
    int r;
    if (!get_format(env, argv[0], &format))
	return enif_make_badarg(env);
    if ((r = snd_pcm_format_physical_width(format)) < 0)
	return make_error(env, enif_make_int(env, r));
    return enif_make_int(env, r);    
}

static ERL_NIF_TERM nif_format_size(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[]) {

    UNUSED(argc);
    snd_pcm_format_t format;
    ssize_t r;
    unsigned long samples;
    if (!get_format(env, argv[0], &format))
	return enif_make_badarg(env);
    if (!enif_get_ulong(env, argv[1], &samples) || (samples <= 0))
	return enif_make_badarg(env);    
    if ((r = snd_pcm_format_size(format, samples)) < 0)
	return make_error(env, enif_make_int(env, r));
    return enif_make_int(env, r);        
}

static ERL_NIF_TERM nif_format_silence(ErlNifEnv* env, int argc,
				       const ERL_NIF_TERM argv[]) {
    UNUSED(argc);
    snd_pcm_format_t format;
    unsigned char* dst;    
    uint64_t silence;
    ERL_NIF_TERM result;
    
    if (!get_format(env, argv[0], &format))
	return enif_make_badarg(env);
    silence = snd_pcm_format_silence_64(format);
    dst = enif_make_new_binary(env, 8, &result);
    memcpy(dst, (void*)&silence, 8);
    return result;
}

static ERL_NIF_TERM nif_make_silence(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[]) {
    UNUSED(argc);
    snd_pcm_format_t format;
    unsigned char* dst;    
    unsigned int samples;
    unsigned int channels;
    ssize_t r;
    ERL_NIF_TERM result;
    
    if (!get_format(env, argv[0], &format))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[1], &channels))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[2], &samples))
	return enif_make_badarg(env);
    if ((r = snd_pcm_format_size(format, channels*samples)) < 0)
    	return enif_make_badarg(env);
    dst = enif_make_new_binary(env, (size_t)r, &result);
    if ((r = snd_pcm_format_set_silence(format, dst, channels*samples)) < 0)
	return make_error(env, enif_make_int(env, r));
    return result;
}

/*
 * dtor
 */

void pcm_dtor(ErlNifEnv *env, void *obj) {
    handle_t *handle = (handle_t *)obj;
    if (handle->is_open) {
	int i;
	DEBUGF("pcm_dtor: (open) access_count = %d", handle->access_count);
	handle->is_open = 0;	
	for (i = 0; i < handle->nfds; i++) {
	    enif_select(env, (ErlNifEvent)handle->fds[i].fd,
			ERL_NIF_SELECT_CANCEL,
			handle, NULL, ATOM(undefined));
	}
    }
    else {
	DEBUGF("dtor (closed) access_count = %d", handle->access_count);
    }
    enif_mutex_destroy(handle->access_mtx);
}

static void pcm_stop(ErlNifEnv* env, handle_t* handle,
		     ErlNifEvent event, int is_direct_call)
{
    UNUSED(env);
    int i, j;
    DEBUGF("pcm_stop: event=%lx\r\n", (intptr_t)event);
    j = handle->nfds-1;
    for (i = 0; i <= j; i++) {
	if (handle->fds[i].fd == (ErlNifEvent)event) {
	    handle->fds[i] = handle->fds[j];
	    handle->nfds--;
	    if ((handle->nfds == 0) && (!handle->is_open)) {
		DEBUGF("pcm_stop: close pcm%s", "");
		snd_pcm_close(handle->pcm_handle);
	    }
	    return;
	}
    }
}

static void pcm_down(ErlNifEnv* env, handle_t* handle,
		     const ErlNifPid* pid, const ErlNifMonitor* mon)
{
    UNUSED(env);
    DEBUGF("pcm_down nfds=%d", handle->nfds);
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
    LOAD_ATOM(undefined);
    LOAD_ATOM(true);
    LOAD_ATOM(false);    
    LOAD_ATOM(ok);
    LOAD_ATOM(error);

    // open mode
    LOAD_ATOM(playback);
    LOAD_ATOM(capture);

    // hw-parameters
    LOAD_ATOM(format);
    LOAD_ATOM(channels);
    LOAD_ATOM(rate);
    LOAD_ATOM(period_size);
    LOAD_ATOM(buffer_size);
    // sw-parameters
    LOAD_ATOM(start_threshold);
    
    LOAD_ATOM(underrun);
    LOAD_ATOM(overrun);
    LOAD_ATOM(suspend_event);
    LOAD_ATOM(would_block);
    LOAD_ATOM(system_call);

    // posix errors
    LOAD_ATOM(eagain);
    LOAD_ATOM(eintr);
    LOAD_ATOM(estrpipe);
    LOAD_ATOM(epipe);

    // errors
    LOAD_ATOM(bad_param);
    LOAD_ATOM(no_such_handle);
    LOAD_ATOM(select_already);
    
    // state
    LOAD_ATOM(open);
    LOAD_ATOM(setup);	
    LOAD_ATOM(prepared);
    LOAD_ATOM(running);
    LOAD_ATOM(xrun);
    LOAD_ATOM(draining);
    LOAD_ATOM(paused);	
    LOAD_ATOM(suspended);	
    LOAD_ATOM(disconnected);
    LOAD_ATOM(private1);

    // format
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
    
    return 0;
}


static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info); 
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit cb;
    
    cb.dtor = (ErlNifResourceDtor*) pcm_dtor;
    cb.stop = (ErlNifResourceStop*) pcm_stop;
    cb.down = (ErlNifResourceDown*) pcm_down;
    
    if ((handle_resource_type =
         enif_open_resource_type_x(env, "alsa_nif", &cb,
				   ERL_NIF_RT_CREATE,
				   &tried)) == NULL) {
        return -1;
    }
    if (load_atoms(env) < 0)
	return -1;

    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit cb;
    
    cb.dtor = (ErlNifResourceDtor*) pcm_dtor;
    cb.stop = (ErlNifResourceStop*) pcm_stop;
    cb.down = (ErlNifResourceDown*) pcm_down;

    DEBUGF("upgrade%s", "");

    if ((handle_resource_type =
	 enif_open_resource_type_x(env, "alsa_nif", &cb,
				   ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
				   &tried)) == NULL) {
	return -1;
    }
    if (load_atoms(env) < 0)
	return -1;
    *priv_data = *old_priv_data;
    return 0;
}

/*
 * unload
 */

static void unload(ErlNifEnv* env, void* priv_data) {
}

/*
 * NIF functions registration
 */


ERL_NIF_INIT(alsa, nif_funcs, load, NULL, upgrade, unload);
