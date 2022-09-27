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

#include "wave.h"
#include "reformat.h"


extern void mix(snd_pcm_format_t format, void** srcp, size_t num_channels,
		void* dst, size_t num_frames);

extern void resample(snd_pcm_format_t format, size_t channels, 
		     size_t src_rate, void* src,
		     size_t dst_rate, void* dst,
		     size_t num_frames, size_t max_dst_frames);

#define MAX_CHANNELS 8  // current "soft" max in frames
#define MAX_VOICES  32  // current "soft" max of voices for mix

#define UNUSED(a) ((void) a)

#ifdef DEBUG
#define DEBUGF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#else
#define DEBUGF(f,a...)
#endif
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
// forms
DECL_ATOM(const);
DECL_ATOM(sine);
DECL_ATOM(square);
DECL_ATOM(triangle);
DECL_ATOM(saw);
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
    NIF("mix", 4,  nif_mix)		  \
    NIF("resample", 5, nif_resample)	  \
    NIF("reformat", 5, nif_reformat)	  \
    NIF("wave_new", 0, nif_wave_new)				\
    NIF("wave",     4, nif_wave)				\
    NIF("wave_set_envelope", 6, nif_wave_set_envelope)		\
    NIF("wave_set_level", 5, nif_wave_set_level)		\
    NIF("wave_set_wave", 10, nif_wave_set_wave)			\
    NIF("wave_set_rate", 2, nif_wave_set_rate)			\
    NIF("wave_set_nwaves", 2, nif_wave_set_nwaves)		\
    NIF("wave_set_time", 2, nif_wave_set_time)			\
    NIF("wave_set_attack", 3, nif_wave_set_attack)		\
    NIF("wave_set_decay", 3, nif_wave_set_decay)		\
    NIF("wave_set_sustain", 3, nif_wave_set_sustain)		\
    NIF("wave_set_release", 3, nif_wave_set_release)		\
    NIF("wave_set_low", 3, nif_wave_set_low)			\
    NIF("wave_set_peek", 3, nif_wave_set_peek)			\
    NIF("wave_set_sust", 3, nif_wave_set_sust)			\
    NIF("wave_set_form", 3, nif_wave_set_form)			\
    NIF("wave_set_env", 3, nif_wave_set_env)			\
    NIF("wave_set_chan", 3, nif_wave_set_chan)			\
    NIF("wave_set_f1", 3, nif_wave_set_f1)			\
    NIF("wave_set_f2", 3, nif_wave_set_f2)			\
    NIF("wave_set_f3", 3, nif_wave_set_f3)			\
    NIF("wave_set_f4", 3, nif_wave_set_f4)			\
    NIF("wave_set_f5", 3, nif_wave_set_f5)			\
    NIF("wave_set_phase", 3, nif_wave_set_phase)		\
    NIF("wave_set_noice", 3, nif_wave_set_noice)

typedef struct _helem_t {
    struct _helem_t* next;  // next in chain    
    ERL_NIF_TERM* atm_ptr;  // the hash atom
    unsigned int  enm;      // enumerated value
    unsigned int  hval;     // hash value
} helem_t;

ErlNifResourceType *wavedef_r;

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

/*
static ERL_NIF_TERM make_error(ErlNifEnv* env, int err)
{
    DEBUGF("make_error: %d", err);
    return enif_make_tuple2(env,
			    ATOM(error),
			    enif_make_atom(env, erl_errno_id(err)));
}
*/

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

// mix(Format, Channels, [binary()], [control()]) -> binary()
static ERL_NIF_TERM nif_mix(ErlNifEnv* env, int argc,
			    const ERL_NIF_TERM argv[])
{
    snd_pcm_format_t format;
    size_t num_channels;
    ERL_NIF_TERM list;
    ERL_NIF_TERM dst_bin;
    void* dst;
    void* voice[MAX_VOICES];
    ErlNifBinary voice_bin[MAX_VOICES];
    unsigned num_voices;
    unsigned num_controls;
    size_t num_frames;
    size_t num_samples;
    size_t frame_size;
    size_t voice_size = 0;
    int i;
    
    if (!get_format(env, argv[0], &format))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[1], &num_channels))
	return enif_make_badarg(env);
    list = argv[2];
    if (!enif_get_list_length(env, list, &num_voices))
	return enif_make_badarg(env);
    if (num_voices == 0)
	return enif_make_list(env, 0);
    if (num_voices > MAX_VOICES)
	return enif_make_badarg(env);
    for (i = 0; i < (int)num_voices; i++) {
	ERL_NIF_TERM hd, tl;
	enif_get_list_cell(env, list, &hd, &tl);
	if (!enif_inspect_binary(env, hd, &voice_bin[i]))
	    return enif_make_badarg(env);
	voice[i] = voice_bin[i].data;
	if (i == 0)
	    voice_size = voice_bin[i].size;
	else if (voice_bin[i].size != voice_size)
	    return enif_make_badarg(env);
	list = tl;
    }
    list = argv[3];
    if (!enif_get_list_length(env, list, &num_controls))
	return enif_make_badarg(env);
    frame_size = num_channels * snd_pcm_format_size(format, 1);
    num_frames = voice_size / frame_size;

    dst = enif_make_new_binary(env, voice_size, &dst_bin);
    num_samples = num_frames*num_channels;
    mix(format, voice, num_voices, dst, num_samples);

    return dst_bin;
}

// resample(SrcRate, DstRate, Format, Channels, Src::binary()) -> Dst:binary().
static ERL_NIF_TERM nif_resample(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    snd_pcm_format_t format;
    size_t src_rate;
    size_t dst_rate;
    size_t frame_size;
    size_t dst_size;
    size_t src_frames;
    size_t dst_frames;
    size_t num_channels;
    double dx;
    ErlNifBinary src;
    ERL_NIF_TERM dst_bin;    
    void* dst;
    
    if (!get_size_t(env, argv[0], &src_rate))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[1], &dst_rate))
	return enif_make_badarg(env);
    if (!get_format(env, argv[2], &format))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[3], &num_channels))
	return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[4], &src))
	return enif_make_badarg(env);
        
    frame_size = num_channels * snd_pcm_format_size(format, 1);
    src_frames = src.size / frame_size;
    dx = (double) dst_rate / (double) src_rate;
    dst_frames = dx * src_frames;
    dst_size = dst_frames * frame_size;
    dst = enif_make_new_binary(env, dst_size, &dst_bin);

    resample(format, num_channels,
	     src_rate, src.data,
	     dst_rate, dst,
	     src_frames, dst_frames);
    
    return dst_bin;
}

// reformat(SrcFormat, DstFormat, SrcChannels, DstChannels, Src::binary() ->
//      Dst::binary().
static ERL_NIF_TERM nif_reformat(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    snd_pcm_format_t src_format;
    snd_pcm_format_t dst_format;
    size_t src_channels;
    size_t dst_channels;
    ErlNifBinary src;
    ERL_NIF_TERM dst_bin;
    void* dst;
    size_t frame_size;
    size_t dst_size;
    size_t num_frames;
    
    if (!get_format(env, argv[0], &src_format))
	return enif_make_badarg(env);
    if (!get_format(env, argv[1], &dst_format))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[2], &src_channels))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[3], &dst_channels))
	return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[4], &src))
	return enif_make_badarg(env);    

    frame_size = src_channels * snd_pcm_format_size(src_format, 1);
    num_frames = src.size / frame_size;
    dst_size = num_frames * dst_channels * snd_pcm_format_size(dst_format, 1);

    dst = enif_make_new_binary(env, dst_size, &dst_bin);

    reformat(src_format, src_channels, src.data,
	     dst_format, dst_channels, dst,
	     num_frames);
    return dst_bin;
}

static ERL_NIF_TERM nif_wave_new(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    wavedef_t *param = enif_alloc_resource(wavedef_r, sizeof(wavedef_t));
    ERL_NIF_TERM term;
    wave_init(param);
    term = enif_make_resource(env, param);
    enif_release_resource(param);
    return term;
}

// set enelope adsr params
static ERL_NIF_TERM nif_wave_set_envelope(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double attack, decay, sustain, release;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[2], &attack))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[3], &decay))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[4], &sustain))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[5], &release))
	return enif_make_badarg(env);
    if (wave_set_envelope(param, index, attack, decay, sustain, release) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

// set envelope levels
static ERL_NIF_TERM nif_wave_set_level(ErlNifEnv* env, int argc,
				       const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double low, peek, sust;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[2], &low))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[3], &peek))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[4], &sust))
	return enif_make_badarg(env);
    if (wave_set_level(param, index, low, peek, sust) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_wave(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    waveform_t form;
    double f1, f2, f3, f4, f5, phase, noice;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (argv[2] == ATOM(sine))
	form = SINE;
    else if (argv[2] == ATOM(square))
	form = SQUARE;
    else if (argv[2] == ATOM(triangle))
	form = TRIANGLE;
    else if (argv[2] == ATOM(saw))
	form = SAW;
    else if (argv[2] == ATOM(const))
	form = CONST;    
    else
	return enif_make_badarg(env);    
    if (!get_number(env, argv[3], &f1))
	return enif_make_badarg(env);
    if (!get_number(env, argv[4], &f2))
	return enif_make_badarg(env);
    if (!get_number(env, argv[5], &f3))
	return enif_make_badarg(env);
    if (!get_number(env, argv[6], &f4))
	return enif_make_badarg(env);
    if (!get_number(env, argv[7], &f5))
	return enif_make_badarg(env);
    if (!get_number(env, argv[8], &phase))
	return enif_make_badarg(env);
    if (!get_number(env, argv[9], &noice))
	return enif_make_badarg(env);
    if (wave_set_wave(param, index, form, f1, f2, f3, f4, f5,
		      phase, noice) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_rate(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    unsigned int rate;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[1], &rate))
	return enif_make_badarg(env);
    if (wave_set_rate(param, rate) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_nwaves(ErlNifEnv* env, int argc,
					const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    unsigned int nwaves;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[1], &nwaves))
	return enif_make_badarg(env);
    if (wave_set_nwaves(param, nwaves) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_time(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    double new_time;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!get_number(env, argv[1], &new_time))
	return enif_make_badarg(env);
    if (wave_set_time(param, new_time) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}


static ERL_NIF_TERM nif_wave_set_attack(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &value))
	return enif_make_badarg(env);
    if (wave_set_attack(param, index, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_decay(ErlNifEnv* env, int argc,
				       const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &value))
	return enif_make_badarg(env);
    if (wave_set_decay(param, index, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_sustain(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &value))
	return enif_make_badarg(env);
    if (wave_set_sustain(param, index, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_release(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &value))
	return enif_make_badarg(env);
    if (wave_set_release(param, index, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_low(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[2], &value))
	return enif_make_badarg(env);
    if (wave_set_low(param, index, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_peek(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[2], &value))
	return enif_make_badarg(env);
    if (wave_set_peek(param, index, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_sust(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[2], &value))
	return enif_make_badarg(env);
    if (wave_set_sust(param, index, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}


static ERL_NIF_TERM nif_wave_set_form(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    waveform_t form;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!enif_is_atom(env, argv[2]))
	return enif_make_badarg(env);
    if (argv[2] == ATOM(sine))
	form = SINE;
    else if (argv[2] == ATOM(square))
	form = SQUARE;
    else if (argv[2] == ATOM(triangle))
	form = TRIANGLE;
    else if (argv[2] == ATOM(saw))
	form = SAW;
    else if (argv[2] == ATOM(const))
	form = CONST;    
    else
	return enif_make_badarg(env);
    if (wave_set_form(param, index, form) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}


static ERL_NIF_TERM nif_wave_set_env(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    unsigned int env_index;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[2], &env_index))
	return enif_make_badarg(env);
    if (wave_set_env(param, index, env_index) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_chan(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    unsigned int chan;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[2], &chan))
	return enif_make_badarg(env);
    if (wave_set_chan(param, index, chan) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_f1(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &value))
	return enif_make_badarg(env);
    if (wave_set_f1(param, index, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_f2(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &value))
	return enif_make_badarg(env);
    if (wave_set_f2(param, index, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_f3(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &value))
	return enif_make_badarg(env);
    if (wave_set_f3(param, index, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_f4(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &value))
	return enif_make_badarg(env);
    if (wave_set_f4(param, index, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_f5(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &value))
	return enif_make_badarg(env);
    if (wave_set_f5(param, index, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_phase(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &value))
	return enif_make_badarg(env);
    if (wave_set_phase(param, index, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_noice(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &value))
	return enif_make_badarg(env);
    if (wave_set_noice(param, index, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

// wave(WaveDef,Format,Channels,NumFrames::non_neg_integer()) -> Dst:binary().
static ERL_NIF_TERM nif_wave(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    snd_pcm_format_t format;
    size_t num_channels;
    size_t num_frames;
    size_t frame_size;
    size_t size;
    ERL_NIF_TERM dst_bin;
    void* dst;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!get_format(env, argv[1], &format))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[2], &num_channels))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[3], &num_frames))
	return enif_make_badarg(env);    
    frame_size = num_channels * snd_pcm_format_size(format, 1);
    size = frame_size*num_frames;
    dst = enif_make_new_binary(env, size, &dst_bin);

    enif_fprintf(stderr, "num_frames=%d, frame_size=%d, size=%d\r\n",
		 (int)num_frames, (int)frame_size, (int)size);
    
    wave_buffer(param, format, num_channels, dst, num_frames);

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

    // form
    LOAD_ATOM(const);
    LOAD_ATOM(sine);
    LOAD_ATOM(square);
    LOAD_ATOM(triangle);
    LOAD_ATOM(saw); 

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

void wave_dtor(ErlNifEnv *env, void *obj)
{
    wavedef_t *wp = (wavedef_t *)obj;
    (void) wp;
    DEBUGF("dtor wavdef%s", "");
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
    
    if ((wavedef_r =
         enif_open_resource_type_x(env, "wavedef", &cb,
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

    if ((wavedef_r =
	 enif_open_resource_type_x(env, "wavedef", &cb,
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

ERL_NIF_INIT(alsa_samples, nif_funcs, load, NULL, upgrade, unload)
