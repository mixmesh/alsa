#include <erl_nif.h>
#include <alsa/asoundlib.h>

// #define DEBUGF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#define DEBUGF(f,a...)
#define ERRORF(f,a...) enif_fprintf(stderr, f "\r\n", a)
// #define ERRORF(f,a...)

#define UNUSED(a) ((void) a)
#define MAX_READ_BUFFER_SIZE 65536
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
DECL_ATOM(current);
DECL_ATOM(any);

DECL_ATOM(playback);
DECL_ATOM(capture);
// hw params
DECL_ATOM(format);
DECL_ATOM(formats);
DECL_ATOM(channels);
DECL_ATOM(channels_min);
DECL_ATOM(channels_max);
DECL_ATOM(rate);
DECL_ATOM(rate_min);
DECL_ATOM(rate_max);
DECL_ATOM(period_size);
DECL_ATOM(period_size_min);
DECL_ATOM(period_size_max);
DECL_ATOM(buffer_size);
// hw-get-only
DECL_ATOM(is_double);
DECL_ATOM(is_half_duplex);
DECL_ATOM(can_pause);
DECL_ATOM(can_resume);
DECL_ATOM(can_sync_start);
DECL_ATOM(can_disable_period_wakeup);
DECL_ATOM(fifo_size);
// info
DECL_ATOM(id);
DECL_ATOM(driver);
DECL_ATOM(name);
DECL_ATOM(longname);
DECL_ATOM(mixername);
DECL_ATOM(components);

// sw params
DECL_ATOM(start_threshold);
DECL_ATOM(avail_min);

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
    NIF("pause_",    2, nif_pause) \
    NIF("resume_",   1, nif_resume) \
    NIF("avail_",    1, nif_avail) \
    NIF("state_",    1, nif_state) \
    NIF("strerror",  1, nif_strerror) \
    NIF("get_hw_params_", 2, nif_get_hw_params) \
    NIF("get_hw_params_", 3, nif_get_hw_params) \
    NIF("set_hw_params_", 2, nif_set_hw_params) \
    NIF("get_sw_params_", 2, nif_get_sw_params) \
    NIF("set_sw_params_", 2, nif_set_sw_params) \
    NIF("select_",        1, nif_select) \
    NIF("format_is_signed", 1, nif_format_is_signed) \
    NIF("format_is_unsigned", 1, nif_format_is_unsigned) \
    NIF("format_is_little", 1, nif_format_is_little) \
    NIF("format_is_big", 1, nif_format_is_big) \
    NIF("format_is_linear", 1, nif_format_is_linear) \
    NIF("format_is_float", 1, nif_format_is_float) \
    NIF("format_width", 1, nif_format_width) \
    NIF("format_physical_width", 1, nif_format_physical_width) \
    NIF("format_size", 2, nif_format_size) \
    NIF("format_silence", 1, nif_format_silence) \
    NIF("make_silence", 3, nif_make_silence) \
    NIF("bytes_to_frames", 2, nif_bytes_to_frames) \
    NIF("card_info", 2, nif_card_info) \
    NIF("card_next", 1, nif_card_next)

typedef struct _helem_t {
    struct _helem_t* next;  // next in chain    
    ERL_NIF_TERM* atm_ptr;  // the hash atom
    unsigned int  enm;      // enumerated value
    unsigned int  hval;     // hash value
} helem_t;

static unsigned int hash_atom(ERL_NIF_TERM term)
{
    return (term >> 6);
}

static void hash_helems(char* hname, helem_t** hvec, size_t hsize,
			helem_t* elems)
{
    int i = 0;

    DEBUGF("hash table %s size %u", hname, hsize);
    memset(hvec, 0, hsize*sizeof(helem_t*));

    while(elems[i].atm_ptr != NULL) {
	unsigned int hval = hash_atom(*elems[i].atm_ptr);
	unsigned int ix = hval % hsize;
	if (hvec[i] != NULL) DEBUGF("hash conflict %d", i);
	elems[i].next = hvec[ix];
	elems[i].hval = hval;
	hvec[ix] = &elems[i];
	i++;
    }
}

static int lookup_atom(helem_t** hvec, size_t hsize, ERL_NIF_TERM arg)
{
    unsigned int hval = hash_atom(arg);
    unsigned int ix = hval % hsize;
    helem_t* ptr = hvec[ix];
    while(ptr) {
	if (*ptr->atm_ptr == arg)
	    return (int) ptr->enm;
	ptr = ptr->next;
    }
    return -1;
}

#define HELEM(a, e) { .next = NULL, .atm_ptr = &(a), .enm = (e), .hval = 0}

enum {
    HW_FORMAT,
    HW_FORMATS,
    HW_RATE,
    HW_RATE_MIN,
    HW_RATE_MAX,    
    HW_CHANNELS,
    HW_CHANNELS_MIN,
    HW_CHANNELS_MAX,
    HW_PERIOD_SIZE,
    HW_PERIOD_SIZE_MIN,
    HW_PERIOD_SIZE_MAX,
    HW_BUFFER_SIZE,
    HW_IS_DOUBLE,
    HW_IS_HALF_DUPLEX,
    HW_CAN_PAUSE,
    HW_CAN_RESUME,
    HW_CAN_SYNC_START,
    HW_CAN_DISABLE_PERIOD_WAKEUP,
    HW_FIFO_SIZE,
    HW_LAST
};

#define MAX_HW_PARAMS (2*HW_LAST)

helem_t hw_elems[] =
{
    HELEM(ATOM(format), HW_FORMAT),
    HELEM(ATOM(formats), HW_FORMATS),
    HELEM(ATOM(rate), HW_RATE),
    HELEM(ATOM(rate_min), HW_RATE_MIN),
    HELEM(ATOM(rate_max), HW_RATE_MAX),    
    HELEM(ATOM(channels), HW_CHANNELS),
    HELEM(ATOM(channels_min), HW_CHANNELS_MIN),
    HELEM(ATOM(channels_max), HW_CHANNELS_MAX),
    HELEM(ATOM(period_size), HW_PERIOD_SIZE),
    HELEM(ATOM(period_size_min), HW_PERIOD_SIZE_MIN),
    HELEM(ATOM(period_size_max), HW_PERIOD_SIZE_MAX),
    HELEM(ATOM(buffer_size), HW_BUFFER_SIZE),
    HELEM(ATOM(is_double), HW_IS_DOUBLE),
    HELEM(ATOM(is_half_duplex), HW_IS_HALF_DUPLEX),
    HELEM(ATOM(can_pause), HW_CAN_PAUSE),
    HELEM(ATOM(can_resume), HW_CAN_RESUME),
    HELEM(ATOM(can_sync_start), HW_CAN_SYNC_START),
    HELEM(ATOM(can_disable_period_wakeup), HW_CAN_DISABLE_PERIOD_WAKEUP),
    HELEM(ATOM(fifo_size), HW_FIFO_SIZE),
    { .next = NULL, .atm_ptr = NULL, .enm = 0, .hval = 0}
};

enum {
    SW_START_THRESHOLD,
    SW_AVAIL_MIN,
    SW_LAST
};

#define MAX_SW_PARAMS (2*SW_LAST)

helem_t sw_elems[] =
{
    HELEM(ATOM(start_threshold), SW_START_THRESHOLD),
    HELEM(ATOM(avail_min), SW_AVAIL_MIN),
    { .next = NULL, .atm_ptr = NULL, .enm = 0, .hval = 0}
};


helem_t format_elems[] =
{
    HELEM(ATOM(s8), SND_PCM_FORMAT_S8),
    HELEM(ATOM(u8), SND_PCM_FORMAT_U8),
    HELEM(ATOM(s16_le), SND_PCM_FORMAT_S16_LE),
    HELEM(ATOM(s16_be), SND_PCM_FORMAT_S16_BE),
    HELEM(ATOM(u16_le), SND_PCM_FORMAT_U16_LE),
    HELEM(ATOM(u16_be), SND_PCM_FORMAT_U16_BE),
    HELEM(ATOM(s24_le), SND_PCM_FORMAT_S24_LE),
    HELEM(ATOM(s24_be), SND_PCM_FORMAT_S24_BE),
    HELEM(ATOM(u24_le), SND_PCM_FORMAT_U24_LE),
    HELEM(ATOM(u24_be), SND_PCM_FORMAT_U24_BE),
    HELEM(ATOM(s32_le), SND_PCM_FORMAT_S32_LE),
    HELEM(ATOM(s32_be), SND_PCM_FORMAT_S32_BE),
    HELEM(ATOM(u32_le), SND_PCM_FORMAT_U32_LE),
    HELEM(ATOM(u32_be), SND_PCM_FORMAT_U32_BE),
    HELEM(ATOM(float_le), SND_PCM_FORMAT_FLOAT_LE),
    HELEM(ATOM(float_be), SND_PCM_FORMAT_FLOAT_BE),
    HELEM(ATOM(float64_le), SND_PCM_FORMAT_FLOAT64_LE),
    HELEM(ATOM(float64_be), SND_PCM_FORMAT_FLOAT64_BE),
    HELEM(ATOM(iec958_subframe_le), SND_PCM_FORMAT_IEC958_SUBFRAME_LE),
    HELEM(ATOM(iec958_subframe_be), SND_PCM_FORMAT_IEC958_SUBFRAME_BE),
    HELEM(ATOM(mu_law), SND_PCM_FORMAT_MU_LAW),
    HELEM(ATOM(a_law), SND_PCM_FORMAT_A_LAW),
    HELEM(ATOM(ima_adpcm), SND_PCM_FORMAT_IMA_ADPCM),
    HELEM(ATOM(mpeg), SND_PCM_FORMAT_MPEG),
    HELEM(ATOM(gsm), SND_PCM_FORMAT_GSM),
    HELEM(ATOM(s20_le), SND_PCM_FORMAT_S20_LE),
    HELEM(ATOM(s20_be), SND_PCM_FORMAT_S20_BE),
    HELEM(ATOM(u20_le), SND_PCM_FORMAT_U20_LE),
    HELEM(ATOM(u20_be), SND_PCM_FORMAT_U20_BE),
    HELEM(ATOM(special), SND_PCM_FORMAT_SPECIAL),
    HELEM(ATOM(s24_3le), SND_PCM_FORMAT_S24_3LE),
    HELEM(ATOM(s24_3be), SND_PCM_FORMAT_S24_3BE),
    HELEM(ATOM(u24_3le), SND_PCM_FORMAT_U24_3LE),
    HELEM(ATOM(u24_3be), SND_PCM_FORMAT_U24_3BE),
    HELEM(ATOM(s20_3le), SND_PCM_FORMAT_S20_3LE),
    HELEM(ATOM(s20_3be), SND_PCM_FORMAT_S20_3BE),
    HELEM(ATOM(u20_3le), SND_PCM_FORMAT_U20_3LE),
    HELEM(ATOM(u20_3be), SND_PCM_FORMAT_U20_3BE),
    HELEM(ATOM(s18_3le), SND_PCM_FORMAT_S18_3LE),
    HELEM(ATOM(s18_3be), SND_PCM_FORMAT_S18_3BE),
    HELEM(ATOM(u18_3le), SND_PCM_FORMAT_U18_3LE),
    HELEM(ATOM(u18_3be), SND_PCM_FORMAT_U18_3BE),
    HELEM(ATOM(g723_24), SND_PCM_FORMAT_G723_24),
    HELEM(ATOM(g723_24_1b), SND_PCM_FORMAT_G723_24_1B),
    HELEM(ATOM(g723_40), SND_PCM_FORMAT_G723_40),
    HELEM(ATOM(g723_40_1b), SND_PCM_FORMAT_G723_40_1B),
    HELEM(ATOM(dsd_u8), SND_PCM_FORMAT_DSD_U8),
    HELEM(ATOM(dsd_u16_le), SND_PCM_FORMAT_DSD_U16_LE),
    HELEM(ATOM(dsd_u32_le), SND_PCM_FORMAT_DSD_U32_LE),
    HELEM(ATOM(dsd_u16_be), SND_PCM_FORMAT_DSD_U16_BE),
    HELEM(ATOM(dsd_u32_be), SND_PCM_FORMAT_DSD_U32_BE),
    { .next = NULL, .atm_ptr = NULL, .enm = 0, .hval = 0}    
};


enum {
    INFO_ID,
    INFO_DRIVER,
    INFO_NAME,
    INFO_LONGNAME,
    INFO_MIXERNAME,
    INFO_COMPONENTS,
    INFO_LAST
};

#define MAX_INFO (2*INFO_LAST)

helem_t info_elems[] =
{
    HELEM(ATOM(id), INFO_ID),
    HELEM(ATOM(driver), INFO_DRIVER),
    HELEM(ATOM(name), INFO_NAME),
    HELEM(ATOM(longname), INFO_LONGNAME),
    HELEM(ATOM(mixername), INFO_MIXERNAME),
    HELEM(ATOM(components), INFO_COMPONENTS),
    { .next = NULL, .atm_ptr = NULL, .enm = 0, .hval = 0}
};

#define HW_HASH_SIZE     (2*(sizeof(hw_elems)/sizeof(helem_t))+1)
#define SW_HASH_SIZE     (2*(sizeof(sw_elems)/sizeof(helem_t))+1)
#define FORMAT_HASH_SIZE (2*(sizeof(format_elems)/sizeof(helem_t))+1)
#define INFO_HASH_SIZE   (2*(sizeof(info_elems)/sizeof(helem_t))+1)

typedef struct {
    helem_t* hw_hash[HW_HASH_SIZE];
    helem_t* sw_hash[SW_HASH_SIZE];
    helem_t* format_hash[FORMAT_HASH_SIZE];
    helem_t* info_hash[INFO_HASH_SIZE];
} nif_ctx_t;

#define MAX_NFDS 4  // 4 fd per pcm_handle?

ErlNifResourceType *alsa_r;

typedef struct {
    ErlNifMutex*  access_mtx;
    int           access_count;
    int           is_open;
    snd_pcm_t*    pcm;
    int           nfds;           // number of fds used in select
    int           nstp;           // number of fds stopped
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

static ErlNifFunc nif_funcs[] =
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
    if (!enif_get_resource(env, arg, alsa_r, (void **)&handle))
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
	    snd_pcm_close(handle->pcm);
	}
    }
}

static ERL_NIF_TERM make_boolean(ErlNifEnv* env, int x)
{
    return x ? ATOM(true) : ATOM(false);
}

// Value must be return from NIF
static ERL_NIF_TERM make_result(ErlNifEnv* env, handle_t* handle, ERL_NIF_TERM value) {
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

//
// get_hw_params: given list of params return key value list
//  params is on form [Key::atom() | {Key::atom(),_Value::value}
//
static int get_hw_params(ErlNifEnv* env, snd_pcm_t *pcm,
			 ERL_NIF_TERM list,
			 ERL_NIF_TERM *result,
			 int any) {
    int err;
    snd_pcm_hw_params_t *hw_params;
    ERL_NIF_TERM kv[MAX_HW_PARAMS];
    unsigned int i;
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);    
    
    if ((err = snd_pcm_hw_params_malloc(&hw_params)) < 0) {
	*result = enif_make_badarg(env); // fixme system limit?
	return err;
    }

    if (any)
	err = snd_pcm_hw_params_any(pcm, hw_params);
    else
	err = snd_pcm_hw_params_current(pcm, hw_params);	

    if (err < 0)
	goto error;

    ERL_NIF_TERM head, tail;
    i = 0;
    while(enif_get_list_cell(env, list, &head, &tail) && (i < MAX_HW_PARAMS)) {
	const ERL_NIF_TERM* pair;
	int arity;	
	if (enif_is_tuple(env, head) &&
	    enif_get_tuple(env, head, &arity, &pair) && (arity == 2)) {
	    head = pair[0];
	}
	DEBUGF("get_hw_params: %T", head);

	switch(lookup_atom(ctx->hw_hash, HW_HASH_SIZE, head)) {
	case HW_FORMAT: {
	    snd_pcm_format_t format;
	    err = snd_pcm_hw_params_get_format(hw_params, &format);
	    if (err < 0) goto error;
	    kv[i++] = enif_make_tuple2(env, head, make_format(env, format));
	    break;
	}
	case HW_FORMATS: {
	    snd_pcm_format_t format;
	    ERL_NIF_TERM format_list = enif_make_list(env, 0);
	    for (format = SND_PCM_FORMAT_LAST; format >= 0; format--) {
		if (snd_pcm_hw_params_test_format(pcm, hw_params,
						  format) == 0) {
		    format_list = enif_make_list_cell(env,
						      make_format(env, format),
						      format_list);
		}
	    }
	    kv[i++] = enif_make_tuple2(env, head, format_list);
	    break;
	}
	case HW_CHANNELS: {
	    unsigned int channels;
	    err = snd_pcm_hw_params_get_channels(hw_params, &channels);
	    if (err < 0) goto error;	    
	    kv[i++] = enif_make_tuple2(env, head,
				       enif_make_uint(env, channels));
	    break;
	}
	case HW_CHANNELS_MIN: {
	    unsigned int channels;
	    err = snd_pcm_hw_params_get_channels_min(hw_params, &channels);
	    if (err < 0) goto error;	    
	    kv[i++] = enif_make_tuple2(env, head,
				       enif_make_uint(env, channels));
	    break;
	}
	case HW_CHANNELS_MAX: {
	    unsigned int channels;
	    err = snd_pcm_hw_params_get_channels_min(hw_params, &channels);
	    if (err < 0) goto error;	    
	    kv[i++] = enif_make_tuple2(env, head,
				       enif_make_uint(env, channels));
	    break;
	}	    	    
	case HW_RATE: {
	    int dir;
	    unsigned int rate;
	    err = snd_pcm_hw_params_get_rate(hw_params, &rate, &dir);
	    if (err < 0) goto error;
	    kv[i++] = enif_make_tuple2(env, head,
				       enif_make_uint(env, rate));
	    break;
	}
	case HW_RATE_MIN: {	    
	    int dir;
	    unsigned int rate;
	    err = snd_pcm_hw_params_get_rate_min(hw_params, &rate, &dir);
	    if (err < 0) goto error;
	    kv[i++] = enif_make_tuple2(env, head,
				       enif_make_uint(env, rate));
	    break;	    
	}
	case HW_RATE_MAX: {
	    int dir;
	    unsigned int rate;
	    err = snd_pcm_hw_params_get_rate_max(hw_params, &rate, &dir);
	    if (err < 0) goto error;
	    kv[i++] = enif_make_tuple2(env, head,
				       enif_make_uint(env, rate));
	    break;
	}
	case HW_PERIOD_SIZE: {
	    int dir;	    
	    snd_pcm_uframes_t period_size;
	    err = snd_pcm_hw_params_get_period_size(hw_params,
						    &period_size,&dir);
	    if (err < 0) goto error;
	    kv[i++] = enif_make_tuple2(env, head,
				       enif_make_uint(env, period_size));
	    break;
	}
	case HW_PERIOD_SIZE_MIN: {
	    int dir;	    
	    snd_pcm_uframes_t period_size;
	    err = snd_pcm_hw_params_get_period_size_min(hw_params,
						    &period_size,&dir);
	    if (err < 0) goto error;
	    kv[i++] = enif_make_tuple2(env, head,
				       enif_make_uint(env, period_size));
	    break;
	}
	case HW_PERIOD_SIZE_MAX: {
	    int dir;	    
	    snd_pcm_uframes_t period_size;
	    err = snd_pcm_hw_params_get_period_size_max(hw_params,
						    &period_size,&dir);
	    if (err < 0) goto error;
	    kv[i++] = enif_make_tuple2(env, head,
				       enif_make_uint(env, period_size));
	    break;
	}	    
	case HW_BUFFER_SIZE: {
	    snd_pcm_uframes_t buffer_size;
	    err = snd_pcm_hw_params_get_buffer_size(hw_params, &buffer_size);
	    if (err < 0) goto error;
	    kv[i++] = enif_make_tuple2(env, head,
				       enif_make_uint(env, buffer_size));
	    break;
	}
	case HW_IS_DOUBLE: {
	    int r;
	    r = snd_pcm_hw_params_is_double(hw_params);
	    kv[i++] = enif_make_tuple2(env, head,
				       make_boolean(env, r));
	    break;
	}
	case HW_IS_HALF_DUPLEX: {
	    int r;
	    r = snd_pcm_hw_params_is_half_duplex(hw_params);
	    kv[i++] = enif_make_tuple2(env, head,
				       make_boolean(env, r));
	    break;
	}
	case HW_CAN_PAUSE: {
	    int r;
	    r = snd_pcm_hw_params_can_pause(hw_params);
	    kv[i++] = enif_make_tuple2(env, head,
				       make_boolean(env, r));
	    break;
	}
	case HW_CAN_RESUME: {
	    int r;
	    r = snd_pcm_hw_params_can_resume(hw_params);
	    kv[i++] = enif_make_tuple2(env, head,
				       make_boolean(env, r));
	    break;
	}
	case HW_CAN_SYNC_START: {
	    int r;
	    r = snd_pcm_hw_params_can_sync_start(hw_params);
	    kv[i++] = enif_make_tuple2(env, head,
				       make_boolean(env, r));
	    break;
	}
	case HW_CAN_DISABLE_PERIOD_WAKEUP: {
	    int r;
	    r = snd_pcm_hw_params_can_disable_period_wakeup(hw_params);
	    kv[i++] = enif_make_tuple2(env, head,
				       make_boolean(env, r));
	    break;
	}
	case HW_FIFO_SIZE: {
	    int r;
	    r = snd_pcm_hw_params_get_fifo_size(hw_params);
	    kv[i++] = enif_make_tuple2(env, head,
				       enif_make_int(env, r));
	    break;
	}
	default:
	    DEBUGF("unknown setting%s", "");
	    goto badarg;
	}
	list = tail;
    }
    if (!enif_is_empty_list(env, list))
	goto badarg;
    if (i == MAX_HW_PARAMS)
	goto badarg;
    *result = enif_make_list_from_array(env, kv, i);
    return 1;
badarg:
    snd_pcm_hw_params_free(hw_params);
    *result = enif_make_badarg(env);
    return 0;
error:
    snd_pcm_hw_params_free(hw_params);
    *result = enif_make_int(env, err);
    return err;
}


static int set_hw_params(ErlNifEnv* env, snd_pcm_t *pcm,
			 ERL_NIF_TERM arg,
			 ERL_NIF_TERM* ret) {
    int err;
    snd_pcm_hw_params_t *hw_params;
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    
    if ((err = snd_pcm_hw_params_malloc(&hw_params)) < 0) {
	*ret = enif_make_int(env, err);
	return -1;
    }

    if ((err = snd_pcm_hw_params_any(pcm, hw_params)) < 0) {
        snd_pcm_hw_params_free(hw_params);
	*ret = enif_make_int(env, err);
	return -1;	
    }

    if ((err = snd_pcm_hw_params_set_access(
                   pcm, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED)) <
        0) {
        snd_pcm_hw_params_free(hw_params);
	*ret = enif_make_int(env, err);
	return -1;
    }

    ERL_NIF_TERM list = arg;
    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM* pair;
    int arity;
    while(enif_get_list_cell(env, list, &head, &tail) &&
	  enif_get_tuple(env, head, &arity, &pair) && (arity == 2)) {

	DEBUGF("set_hw_params: %T = %T", pair[0], pair[1]);

	switch(lookup_atom(ctx->hw_hash, HW_HASH_SIZE, pair[0])) {
	case HW_FORMAT: {
	    snd_pcm_format_t format;
	    if (!get_format(env, pair[1], &format)) goto badarg;
	    DEBUGF("format=%d", format);
	    err = snd_pcm_hw_params_set_format(pcm, hw_params,
					       format);
	    if (err < 0) goto error;
	    break;
	}
	case HW_CHANNELS: {
	    unsigned int channels;
	    if (!enif_get_uint(env, pair[1], &channels)) goto badarg;
	    DEBUGF("channels=%u", channels);
	    err = snd_pcm_hw_params_set_channels(pcm, hw_params,
						 channels);
	    if (err < 0) goto error;
	    break;
	}
	case HW_RATE: {
	    unsigned int rate;
	    int dir;
	    if (!enif_get_uint(env, pair[1], &rate)) goto badarg;
	    DEBUGF("rate=%u", rate);
	    err = snd_pcm_hw_params_set_rate_near(pcm,hw_params,
						  &rate, &dir);
	    if (err < 0) goto error;
	    break;	    
	}
	case HW_PERIOD_SIZE: {
	    snd_pcm_uframes_t period_size;
	    int dir;
	    if (!enif_get_ulong(env, pair[1], &period_size)) goto badarg;
	    DEBUGF("period_size=%lu", period_size);
	    err = snd_pcm_hw_params_set_period_size_near(pcm, hw_params,
							 &period_size, &dir);
	    if (err < 0) goto error;
	    break;
	}
	case HW_BUFFER_SIZE: {
	    snd_pcm_uframes_t buffer_size;
	    if (!enif_get_ulong(env, pair[1], &buffer_size)) goto badarg;
	    DEBUGF("buffer_size=%lu", buffer_size);
	    err = snd_pcm_hw_params_set_buffer_size_near(pcm, hw_params,
							 &buffer_size);
	    if (err < 0) goto error;
	    break;
	}
	default:
	    DEBUGF("unknown setting%s", "");
	    goto badarg;
	}
	list = tail;
    }
    if (!enif_is_empty_list(env, list))
	goto badarg;

    err = snd_pcm_hw_params(pcm, hw_params);
    if (err < 0) goto badarg;

    snd_pcm_hw_params_free(hw_params);
    *ret = ATOM(ok);
    return 1;

badarg:
    snd_pcm_hw_params_free(hw_params);
    *ret = enif_make_badarg(env);
    return 0;

error:
    snd_pcm_hw_params_free(hw_params);
    *ret = enif_make_tuple4(env, ATOM(bad_param), pair[0], pair[1],
			    enif_make_int(env, err));
    return 0;
}

static int get_sw_params(ErlNifEnv* env, snd_pcm_t *pcm,
			 ERL_NIF_TERM list,
			 ERL_NIF_TERM *result) {

    int err;
    snd_pcm_sw_params_t *params;
    ERL_NIF_TERM kv[MAX_SW_PARAMS];
    unsigned int i;    
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    
    if ((err = snd_pcm_sw_params_malloc(&params)) < 0) {
	*result = enif_make_badarg(env); // fixme system limit?
	return err;
    }

    if ((err = snd_pcm_sw_params_current(pcm, params)) < 0)
	goto error;

    ERL_NIF_TERM head, tail;
    i = 0;
    while(enif_get_list_cell(env, list, &head, &tail) && (i < MAX_SW_PARAMS)) {
	const ERL_NIF_TERM* pair;
	int arity;
	// allow {key,value} and ignore value 
	if (enif_is_tuple(env, head) &&
	    enif_get_tuple(env, head, &arity, &pair) && (arity == 2)) {
	    head = pair[0];
	}
	DEBUGF("get_sw_params: %T", head);

	switch(lookup_atom(ctx->sw_hash, SW_HASH_SIZE, head)) {	
	case SW_START_THRESHOLD: {
	    snd_pcm_uframes_t start_threshold;
	    err = snd_pcm_sw_params_get_start_threshold(params,
							&start_threshold);
	    if (err < 0) goto error;
	    kv[i++] = enif_make_tuple2(env, head,
				       enif_make_uint(env, start_threshold));
	    break;
	}
	case SW_AVAIL_MIN: {
	    snd_pcm_uframes_t avail_min;
	    err = snd_pcm_sw_params_get_avail_min(params,
						  &avail_min);
	    if (err < 0) goto error;
	    kv[i++] = enif_make_tuple2(env, head,
				       enif_make_uint(env, avail_min));
	    break;
	}    
	default:
	    DEBUGF("unknown setting%s", "");
	    goto badarg;
	}
	list = tail;
    }
    if (!enif_is_empty_list(env, list))
	goto badarg;
    if (i == MAX_SW_PARAMS)
	goto badarg;
    *result = enif_make_list_from_array(env, kv, i);
    return 1;
badarg:
    snd_pcm_sw_params_free(params);
    *result = enif_make_badarg(env);
    return 0;
error:
    snd_pcm_sw_params_free(params);
    *result = enif_make_int(env, err);
    return err;    
}

static int set_sw_params(ErlNifEnv* env, snd_pcm_t *pcm,
			 ERL_NIF_TERM arg,
			 ERL_NIF_TERM* ret) {
    int err;
    snd_pcm_sw_params_t *params;
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    
    if ((err = snd_pcm_sw_params_malloc(&params)) < 0) {
	*ret = enif_make_int(env, err);
	return -1;
    }

    if ((err = snd_pcm_sw_params_current(pcm, params)) < 0) {
        snd_pcm_sw_params_free(params);
	*ret = enif_make_int(env, err);
	return -1;		
    }

    ERL_NIF_TERM list = arg;
    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM* pair;
    int arity;
    while(enif_get_list_cell(env, list, &head, &tail) &&
	  enif_get_tuple(env, head, &arity, &pair) && (arity == 2)) {

	DEBUGF("set_sw_params: %T = %T", pair[0], pair[1]);

	switch(lookup_atom(ctx->sw_hash, SW_HASH_SIZE, pair[0])) {
	case SW_START_THRESHOLD: {
	    snd_pcm_uframes_t start_threshold;
	    if (!enif_get_ulong(env, pair[1], &start_threshold)) goto badarg;
	    DEBUGF("start_threshold=%u", start_threshold);
	    err = snd_pcm_sw_params_set_start_threshold(pcm,params,
							start_threshold);
	    if (err < 0) goto error;
	    break;
	}
	case SW_AVAIL_MIN: {
	    snd_pcm_uframes_t avail_min;
	    if (!enif_get_ulong(env, pair[1], &avail_min)) goto badarg;
	    DEBUGF("avail_min=%u", avail_min);
	    err = snd_pcm_sw_params_set_avail_min(pcm,params,
						  avail_min);
	    if (err < 0) goto error;
	    break;	    
	}    	    
	default:
	    DEBUGF("unknown setting%s", "");
	    goto badarg;
	}
	list = tail;
    }
    if (!enif_is_empty_list(env, list))
	goto badarg;
    
    err = snd_pcm_sw_params(pcm, params);
    if (err < 0) goto badarg;

    snd_pcm_sw_params_free(params);
    *ret = ATOM(ok);
    return 1;    
    
badarg:
    snd_pcm_sw_params_free(params);
    *ret = enif_make_badarg(env);
    return 0;

error:
    snd_pcm_sw_params_free(params);
    *ret = enif_make_tuple4(env, ATOM(bad_param), pair[0], pair[1],
			    enif_make_int(env, err));
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

    snd_pcm_t *pcm;
    if ((err = snd_pcm_open(&pcm,
			    pcm_name,
			    stream,
			    SND_PCM_NONBLOCK
	     )) < 0) {
        return enif_make_tuple2(env, ATOM(error), enif_make_int(env, err));
    }

    handle_t *handle =
        enif_alloc_resource(alsa_r, sizeof(handle_t));
    memset(handle, 0, sizeof(handle_t));
    handle->pcm = pcm;
    handle->access_mtx = enif_mutex_create("pcm_access");
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
    ERL_NIF_TERM value;
    int r;
    int any = 0;

    if (argc >= 3) {
	if (argv[2] == ATOM(any))
	    any = 1;
	else if (argv[2] == ATOM(current))
	    any = 0;
	else
	    return enif_make_badarg(env);
    }
    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    r = get_hw_params(env, handle->pcm, argv[1], &value, any);
    if (r < 0)
	return make_herror(env, handle, value);
    if (r == 0) {
	if (enif_is_exception(env, value)) {
	    done_handle(env, handle);
	    return value;
	}
	return make_herror(env, handle, value);
    }
    return make_result(env, handle, value);
}

/*
 * set_hw_params
 */

static ERL_NIF_TERM nif_set_hw_params(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    int r;
    ERL_NIF_TERM value;
    
    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    r = set_hw_params(env, handle->pcm, argv[1], &value);
    if (r < 0)
	return make_herror(env, handle, value);
    if (r == 0) {
	if (enif_is_exception(env, value)) {
	    done_handle(env, handle);
	    return value;
	}
	return make_herror(env, handle, value);
    }
	    
    r = get_hw_params(env, handle->pcm, argv[1], &value, 0);
    if (r < 0)
	return make_herror(env, handle, value);
    if (r == 0) {
	if (enif_is_exception(env, value)) {
	    done_handle(env, handle);
	    return value;
	}
	return make_herror(env, handle, value);
    }
    return make_result(env, handle, value);
}

/*
 * get_sw_params
 */

static ERL_NIF_TERM nif_get_sw_params(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[]) {

    handle_t *handle;
    ERL_NIF_TERM value;
    int r;
    
    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    r = get_sw_params(env, handle->pcm, argv[1], &value);
    if (r < 0)
	return make_herror(env, handle, value);
    if (r == 0) {
	if (enif_is_exception(env, value)) {
	    done_handle(env, handle);
	    return value;
	}
	return make_herror(env, handle, value);
    }
    return make_result(env, handle, value);
}

/*
 * set_sw_params
 */

static ERL_NIF_TERM nif_set_sw_params(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[]) {

    handle_t *handle;
    int r;
    ERL_NIF_TERM value;
    
    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    r = set_sw_params(env, handle->pcm, argv[1], &value);
    if (r < 0)
	return make_herror(env, handle, value);
    if (r == 0) {
	if (enif_is_exception(env, value)) {
	    done_handle(env, handle);
	    return value;
	}
	return make_herror(env, handle, value);
    }
	    
    r = get_sw_params(env, handle->pcm, argv[1], &value);
    if (r < 0)
	return make_herror(env, handle, value);
    if (r == 0) {
	if (enif_is_exception(env, value)) {
	    done_handle(env, handle);
	    return value;
	}
	return make_herror(env, handle, value);
    }
    return make_result(env, handle, value);
}

/*
 * read
 */

static ERL_NIF_TERM nif_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    snd_pcm_uframes_t nframes;
    ssize_t nbytes;
    unsigned char buf[MAX_READ_BUFFER_SIZE];
    unsigned char* ptr;
    ERL_NIF_TERM data;
    
    if (!enif_get_ulong(env, argv[1], &nframes)) {
        return enif_make_badarg(env);
    }

    handle_t *handle;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }

    nbytes = snd_pcm_frames_to_bytes(handle->pcm, nframes);
    if ((nbytes < 0) || (nbytes >= MAX_READ_BUFFER_SIZE)) {
	nbytes = MAX_READ_BUFFER_SIZE;
	nframes = snd_pcm_bytes_to_frames(handle->pcm, nbytes);
    }

    snd_pcm_sframes_t rframes = snd_pcm_readi(handle->pcm, buf, nframes);
    if (rframes < 0) {
	if (rframes == -EAGAIN)
	    return make_herror(env, handle, ATOM(eagain));
	else if (rframes == -EPIPE)
	    return make_herror(env, handle, ATOM(epipe));
	else if (rframes == -ESTRPIPE)
	    return make_herror(env, handle, ATOM(estrpipe));
	return make_herror(env, handle, enif_make_int(env, rframes));
    }

    // read only frames ready? is that enough to get alsa ticking?
    nbytes = snd_pcm_frames_to_bytes(handle->pcm, rframes);
    ptr = enif_make_new_binary(env, (size_t) nbytes, &data);
    memcpy(ptr, buf, nbytes);
    return make_result(env, handle,
		   enif_make_tuple2(env, enif_make_int(env, rframes), data));
}

/*
 * write
 */

static ERL_NIF_TERM nif_write(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    ErlNifBinary bin;
    
    if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
	return enif_make_badarg(env);

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    snd_pcm_uframes_t frames =
	snd_pcm_bytes_to_frames(handle->pcm, bin.size);
    if (frames == 0)
	return make_result(env, handle, enif_make_int(env, 0));

    snd_pcm_sframes_t written_frames =
	snd_pcm_writei(handle->pcm, bin.data, frames);

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
	snd_pcm_frames_to_bytes(handle->pcm, written_frames);
    return make_result(env, handle, enif_make_int(env, nbytes));
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
    if (snd_pcm_recover(handle->pcm, err, 1) < 0)
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
    
    if ((err = snd_pcm_drain(handle->pcm)) < 0) {
	if (err == -EAGAIN) {
	    snd_pcm_sframes_t remain = snd_pcm_avail(handle->pcm);
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
    
    if ((err = snd_pcm_drop(handle->pcm)) < 0)
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

    if ((err = snd_pcm_prepare(handle->pcm)) < 0)
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
    
    if ((err = snd_pcm_start(handle->pcm)) < 0)
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

    if ((err = snd_pcm_reset(handle->pcm)) < 0)
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

    if ((err = snd_pcm_pause(handle->pcm, enable)) < 0)
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
    
    if ((err = snd_pcm_resume(handle->pcm)) < 0)
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
    
    if ((frames = snd_pcm_avail(handle->pcm)) < 0)
        return make_herror(env, handle, enif_make_long(env, frames));
    return make_result(env, handle, enif_make_long(env, frames));
}

static ERL_NIF_TERM nif_state(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]) {
    handle_t *handle;

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    switch (snd_pcm_state(handle->pcm)) {
    case SND_PCM_STATE_OPEN:
	return make_result(env, handle, ATOM(open));
    case SND_PCM_STATE_SETUP:
	return make_result(env, handle, ATOM(setup));	
    case SND_PCM_STATE_PREPARED:
		return make_result(env, handle, ATOM(prepared));
    case SND_PCM_STATE_RUNNING:
	return make_result(env, handle, ATOM(running));
    case SND_PCM_STATE_XRUN:
	return make_result(env, handle, ATOM(xrun));
    case SND_PCM_STATE_DRAINING:
	return make_result(env, handle, ATOM(draining));
    case SND_PCM_STATE_PAUSED:
	return make_result(env, handle, ATOM(paused));	
    case SND_PCM_STATE_SUSPENDED:
	return make_result(env, handle, ATOM(suspended));	
    case SND_PCM_STATE_DISCONNECTED:
	return make_result(env, handle, ATOM(disconnected));	
    case SND_PCM_STATE_PRIVATE1:
	return make_result(env, handle, ATOM(private1));
    default:
	return make_result(env, handle, ATOM(undefined));
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
    nfds = snd_pcm_poll_descriptors_count(handle->pcm);
    if (nfds > MAX_NFDS) {
	ERRORF("poll_descriptor_count > MAX_NFDS (%d>%d)", nfds, MAX_NFDS);
	nfds = MAX_NFDS;
    }
    handle->nfds = nfds;
    handle->nstp = 0;
    if (nfds > 0) {
	enum ErlNifSelectFlags mode = 0;
	ErlNifPid pid;
	int i;

	handle->nfds = nfds;
	snd_pcm_poll_descriptors(handle->pcm, handle->fds, nfds);

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

static ERL_NIF_TERM nif_format_is_linear(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[]) {
    UNUSED(argc);
    snd_pcm_format_t format;
    int r;
    if (!get_format(env, argv[0], &format))
	return enif_make_badarg(env);
    if ((r = snd_pcm_format_linear(format)) < 0)
	return make_error(env, enif_make_int(env, r));
    return make_boolean(env, r);
}

static ERL_NIF_TERM nif_format_is_float(ErlNifEnv* env, int argc,
					const ERL_NIF_TERM argv[]) {
    UNUSED(argc);
    snd_pcm_format_t format;
    int r;
    if (!get_format(env, argv[0], &format))
	return enif_make_badarg(env);
    if ((r = snd_pcm_format_float(format)) < 0)
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

static ERL_NIF_TERM nif_bytes_to_frames(ErlNifEnv* env, int argc,
					const ERL_NIF_TERM argv[]) {
    handle_t *handle;
    unsigned long bytes;
    
    if (!enif_get_ulong(env, argv[1], &bytes))
	return enif_make_badarg(env);

    switch(get_handle(env, argv[0], &handle)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    
    snd_pcm_uframes_t frames = snd_pcm_bytes_to_frames(handle->pcm, bytes);
    return make_result(env, handle, enif_make_ulong(env, frames));
}

static ERL_NIF_TERM nif_card_info(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[]) {
    int err;
    int   card;
    char  ctl_name[80];
    ERL_NIF_TERM kv[MAX_INFO];
    snd_ctl_t* ctl;    
    snd_ctl_card_info_t* info;
    const char* str;
    ERL_NIF_TERM list;
    ERL_NIF_TERM key, tail;
    ERL_NIF_TERM value;
    nif_ctx_t* ctx;
    int i;

    if (enif_get_int(env, argv[0], &card) < 0)
	return enif_make_badarg(env);
    sprintf(ctl_name, "hw:%d", card);
    if ((err = snd_ctl_open(&ctl, ctl_name, 0)) < 0)
	return enif_make_tuple2(env, ATOM(error), enif_make_int(env, err));

    snd_ctl_card_info_malloc(&info);
    snd_ctl_card_info(ctl, info);
    
    ctx = (nif_ctx_t*) enif_priv_data(env);
    i = 0;
    list = argv[1];
    while(enif_get_list_cell(env, list, &key, &tail) && (i < MAX_INFO)) {
	switch(lookup_atom(ctx->info_hash, INFO_HASH_SIZE, key)) {
	case INFO_ID: {
	    str = snd_ctl_card_info_get_id(info);
	    value = enif_make_string(env, str, ERL_NIF_LATIN1);
	    kv[i++] = enif_make_tuple2(env, key, value);
	    break;
	}
	case INFO_DRIVER: {
	    str = snd_ctl_card_info_get_driver(info);
	    value = enif_make_string(env, str, ERL_NIF_LATIN1);
	    kv[i++] = enif_make_tuple2(env, key, value);	    
	    break;
	}
	case INFO_NAME: {
	    str = snd_ctl_card_info_get_name(info);
	    value = enif_make_string(env, str, ERL_NIF_LATIN1);
	    kv[i++] = enif_make_tuple2(env, key, value);	    
	    break;
	}
	case INFO_LONGNAME: {
	    str = snd_ctl_card_info_get_longname(info);
	    value = enif_make_string(env, str, ERL_NIF_LATIN1);
	    kv[i++] = enif_make_tuple2(env, key, value);	    
	    break;
	}
	case INFO_MIXERNAME: {
	    str = snd_ctl_card_info_get_mixername(info);
	    value = enif_make_string(env, str, ERL_NIF_LATIN1);
	    kv[i++] = enif_make_tuple2(env, key, value);	    
	    break;
	}
	case INFO_COMPONENTS: {
	    str = snd_ctl_card_info_get_components(info);
	    value = enif_make_string(env, str, ERL_NIF_LATIN1);
	    kv[i++] = enif_make_tuple2(env, key, value);	    
	    break;
	}
	default:
	    DEBUGF("unknown setting %T", key);
	    goto badarg;
	}
	list = tail;	    
    }
    if (!enif_is_empty_list(env, list))
	goto badarg;
    if (i == MAX_INFO)
	goto badarg;
    snd_ctl_card_info_free(info);
    snd_ctl_close(ctl);    
    return enif_make_tuple2(env, ATOM(ok),
			    enif_make_list_from_array(env, kv, i));

badarg:
    snd_ctl_card_info_free(info);
    snd_ctl_close(ctl);    
    return enif_make_badarg(env);
    
}
static ERL_NIF_TERM nif_card_next(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[]) {
    UNUSED(env);
    int   card;
    int   err;
    
    if (enif_get_int(env, argv[0], &card) < 0)
	return enif_make_badarg(env);
    if ((err = snd_card_next(&card)) < 0)
	return enif_make_tuple2(env, ATOM(error), enif_make_int(env, err));
    if (card < 0)
	return ATOM(false);
    return enif_make_int(env, card);
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
	if (handle->nfds == 0) {  // not issued a select
	    DEBUGF("pcm_dtor: close pcm%s", "");
	    snd_pcm_close(handle->pcm);
	}
	else {
	    for (i = 0; i < handle->nfds; i++) {
		enif_select(env, (ErlNifEvent)handle->fds[i].fd,
			    ERL_NIF_SELECT_STOP,
			    handle, NULL, ATOM(undefined));
	    }
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
    int i;
    DEBUGF("pcm_stop: direct=%d event=%lx", is_direct_call, (intptr_t)event);

    for (i = 0; i < handle->nfds;  i++) {
	if (handle->fds[i].fd == (ErlNifEvent)event) { // found it
	    handle->fds[i].fd = -1; // mark it as stopped
	    handle->nstp++;
	    if ((handle->nfds == handle->nstp) && !handle->is_open) {
		DEBUGF("pcm_stop: close pcm%s", "");
		snd_pcm_close(handle->pcm);
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
    LOAD_ATOM(current);
    LOAD_ATOM(any);    

    // open mode
    LOAD_ATOM(playback);
    LOAD_ATOM(capture);

    // hw-parameters
    LOAD_ATOM(format);
    LOAD_ATOM(formats);
    LOAD_ATOM(channels);
    LOAD_ATOM(channels_min);
    LOAD_ATOM(channels_max);
    LOAD_ATOM(rate);
    LOAD_ATOM(rate_min);
    LOAD_ATOM(rate_max);    
    LOAD_ATOM(period_size);
    LOAD_ATOM(period_size_min);
    LOAD_ATOM(period_size_max);
    LOAD_ATOM(buffer_size);
    // get-hw-params
    LOAD_ATOM(is_double);
    LOAD_ATOM(is_half_duplex);
    LOAD_ATOM(can_pause);
    LOAD_ATOM(can_resume);
    LOAD_ATOM(can_sync_start);
    LOAD_ATOM(can_disable_period_wakeup);
    LOAD_ATOM(fifo_size);

    // sw-parameters
    LOAD_ATOM(start_threshold);
    LOAD_ATOM(avail_min);
    
    LOAD_ATOM(underrun);
    LOAD_ATOM(overrun);
    LOAD_ATOM(suspend_event);
    LOAD_ATOM(would_block);
    LOAD_ATOM(system_call);

    // info
    LOAD_ATOM(id);
    LOAD_ATOM(driver);
    LOAD_ATOM(name);
    LOAD_ATOM(longname);
    LOAD_ATOM(mixername);
    LOAD_ATOM(components);
    

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
    nif_ctx_t* ctx;
    
    cb.dtor = (ErlNifResourceDtor*) pcm_dtor;
    cb.stop = (ErlNifResourceStop*) pcm_stop;
    cb.down = (ErlNifResourceDown*) pcm_down;

    if ((ctx = (nif_ctx_t*) enif_alloc(sizeof(nif_ctx_t))) == NULL)
	return -1;
    
    if ((alsa_r =
         enif_open_resource_type_x(env, "alsa", &cb,
				   ERL_NIF_RT_CREATE,
				   &tried)) == NULL) {
        return -1;
    }
    if (load_atoms(env) < 0)
	return -1;

    hash_helems("hw", ctx->hw_hash, HW_HASH_SIZE, hw_elems);
    hash_helems("sw", ctx->sw_hash, SW_HASH_SIZE, sw_elems);
    hash_helems("format", ctx->format_hash, FORMAT_HASH_SIZE, format_elems);
    hash_helems("info", ctx->info_hash, INFO_HASH_SIZE, info_elems);
    
    *priv_data = ctx;    
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit cb;
    nif_ctx_t* ctx = (nif_ctx_t*) *old_priv_data;

    cb.dtor = (ErlNifResourceDtor*) pcm_dtor;
    cb.stop = (ErlNifResourceStop*) pcm_stop;
    cb.down = (ErlNifResourceDown*) pcm_down;

    DEBUGF("upgrade%s", "");

    if ((alsa_r =
	 enif_open_resource_type_x(env, "alsa_nif", &cb,
				   ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
				   &tried)) == NULL) {
	return -1;
    }
    if (load_atoms(env) < 0)
	return -1;

    hash_helems("hw", ctx->hw_hash, HW_HASH_SIZE, hw_elems);
    hash_helems("sw", ctx->sw_hash, SW_HASH_SIZE, sw_elems);
    hash_helems("format", ctx->format_hash, FORMAT_HASH_SIZE, format_elems);
    hash_helems("info", ctx->info_hash, INFO_HASH_SIZE, info_elems);
    
    *priv_data = *old_priv_data;
    return 0;
}

/*
 * unload
 */

static void unload(ErlNifEnv* env, void* priv_data) {
    UNUSED(env);
    nif_ctx_t* ctx = (nif_ctx_t*) priv_data;

    DEBUGF("unload%s", "");
    enif_free(ctx);    
}

/*
 * NIF functions registration
 */


ERL_NIF_INIT(alsa, nif_funcs, load, NULL, upgrade, unload);
