//
// ALSA Sample buffer library mixing/resample/reformat
//
#include <stdio.h>
#include <stdint.h>
#include <math.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <alsa/asoundlib.h>

#include "erl_nif.h"
#include "erl_driver.h"

// #define DEBUG
// #define NIF_TRACE

#include "debug.h"

#include "wave.h"
#include "reformat.h"
#include "resample.h"
#include "mix.h"
#include "filt.h"

#include "atom_hash.h"
#include "format.h"

#define MAX_CHANNELS 8  // current "soft" max in frames
#define MAX_VOICES  32  // current "soft" max of voices for mix

#define FORM_BIT  0x01
#define FREQ_BIT  0x02
#define LEVEL_BIT 0x04
#define PHASE_BIT 0x08

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
DECL_ATOM(enoent);
DECL_ATOM(true);
DECL_ATOM(false);
// waveopts
DECL_ATOM(queue);
DECL_ATOM(clear);
DECL_ATOM(buffer_size);
// state
DECL_ATOM(running);
DECL_ATOM(stopped);
// mode
DECL_ATOM(off);
DECL_ATOM(linear);
DECL_ATOM(quadratic);
DECL_ATOM(sustain);
// wave parts
DECL_ATOM(index);
DECL_ATOM(form);
DECL_ATOM(freq);
DECL_ATOM(level);
DECL_ATOM(phase);
// forms
DECL_ATOM(none);
DECL_ATOM(const);
DECL_ATOM(sine);
DECL_ATOM(square);
DECL_ATOM(pulse);
DECL_ATOM(triangle);
DECL_ATOM(saw);
DECL_ATOM(custom);
DECL_ATOM(custom1);
DECL_ATOM(custom2);
DECL_ATOM(custom3);
DECL_ATOM(custom4);
DECL_ATOM(random);

// mark flags
DECL_ATOM(notify);
DECL_ATOM(once);
DECL_ATOM(stop);
DECL_ATOM(set);
DECL_ATOM(repeat);
DECL_ATOM(label);
// wave params
DECL_ATOM(marks);
DECL_ATOM(peak);
DECL_ATOM(energy);
// info atoms
DECL_ATOM(num_samples);
DECL_ATOM(chan);
DECL_ATOM(samples);
DECL_ATOM(osc);
DECL_ATOM(rate);
DECL_ATOM(mute);
DECL_ATOM(state);
DECL_ATOM(t);
DECL_ATOM(dt);
DECL_ATOM(pos);
DECL_ATOM(mode);
DECL_ATOM(envelope);
DECL_ATOM(wave);
DECL_ATOM(mark);

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
    NIF("mix", 4,  nif_mix)		  \
    NIF("resample", 5, nif_resample)	  \
    NIF("reformat", 5, nif_reformat)	  \
    NIF("filter", 4, nif_filter)	  \
    NIF("wave_new", 0, nif_wave_new)				\
    NIF("wave_info", 1, nif_wave_info)				\
    NIF("wave_info", 2, nif_wave_info)				\
    NIF("wave_set_buffer_mode", 3, nif_wave_set_buffer_mode)	\
    NIF("wave_clear", 1, nif_wave_clear)			\
    NIF("wave",     4, nif_wave)				\
    NIF("wave_set_envelope", 2, nif_wave_set_envelope)		\
    NIF("wave_set_adsr", 5, nif_wave_set_adsr)			\
    NIF("wave_set_wave", 3, nif_wave_set_wave)			\
    NIF("wave_set_rate", 2, nif_wave_set_rate)			\
    NIF("wave_get_rate", 1, nif_wave_get_rate)			\
    NIF("wave_set_mode", 2, nif_wave_set_mode)			\
    NIF("wave_set_state", 2, nif_wave_set_state)		\
    NIF("wave_set_mute", 2, nif_wave_set_mute)			\
    NIF("wave_set_time", 2, nif_wave_set_time)			\
    NIF("wave_get_time", 1, nif_wave_get_time)			\
    NIF("wave_set_pos", 2, nif_wave_set_pos)			\
    NIF("wave_get_pos", 1, nif_wave_get_pos)			\
    NIF("wave_set_attack", 2, nif_wave_set_attack)		\
    NIF("wave_set_decay", 2, nif_wave_set_decay)		\
    NIF("wave_set_sustain", 2, nif_wave_set_sustain)		\
    NIF("wave_set_release", 2, nif_wave_set_release)		\
    NIF("wave_set_delay", 2, nif_wave_set_delay)		\
    NIF("wave_get_duration", 1, nif_wave_get_duration)		\
    NIF("wave_set_chan", 3, nif_wave_set_chan)			\
    NIF("wave_set_volume", 3, nif_wave_set_volume)		\
    NIF("wave_set_level", 4, nif_wave_set_level)		\
    NIF("wave_set_form", 4, nif_wave_set_form)			\
    NIF("wave_set_freq", 4, nif_wave_set_freq)			\
    NIF("wave_set_phase", 4, nif_wave_set_phase)		\
    NIF("wave_set_samples", 8, nif_wave_set_samples)		\
    NIF("wave_set_num_samples", 3, nif_wave_set_num_samples)	\
    NIF("wave_get_num_samples", 2, nif_wave_get_num_samples)	\
    NIF("mark", 6, nif_mark)					\
    NIF("unmark", 2, nif_unmark)				\
    NIF("get_marks", 2, nif_get_marks)				\
    NIF("get_marks", 3, nif_get_marks)

ErlNifResourceType *wavedef_r;

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

/*
static ERL_NIF_TERM make_error(ErlNifEnv* env, int err)
{
    DEBUGF("make_error: %d", err);
    return enif_make_tuple2(env,
			    ATOM(error),
			    enif_make_atom(env, erl_errno_id(err)));
}
*/

static int get_boolean(ErlNifEnv* env, ERL_NIF_TERM arg, int* bool)
{
    if (arg == ATOM(true))
	*bool = 1;
    else if (arg == ATOM(false))
	*bool = 0;
    else
	return 0;
    return 1;
}

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

static int get_form(ErlNifEnv* env, ERL_NIF_TERM arg, waveform_t* formp)
{
    if (arg == ATOM(sine))
	*formp = SINE;
    else if (arg == ATOM(square))
	*formp = SQUARE;
    else if (arg == ATOM(pulse))
	*formp = PULSE;    
    else if (arg == ATOM(triangle))
	*formp = TRIANGLE;
    else if (arg == ATOM(saw))
	*formp = SAW;
    else if (arg == ATOM(const))
	*formp = CONST;
    else if (arg == ATOM(none))
	*formp = NONE;
    else if (arg == ATOM(custom))
	*formp = CUSTOM1;
    else if (arg == ATOM(custom1))
	*formp = CUSTOM1;
    else if (arg == ATOM(custom2))
	*formp = CUSTOM2;
    else if (arg == ATOM(custom3))
	*formp = CUSTOM3;
    else if (arg == ATOM(custom4))
	*formp = CUSTOM4;
    else if (arg == ATOM(random))
	*formp = RANDOM;
    else
	return 0;
    return 1;
}

// get custom wave form index or samples index
// -1,-2,-3,-4 -MAX_WAVE_FORMS for wave form index
// 0, 1, 2 ... MAX_WAVES-1 for sample 
static int get_samples_index(ErlNifEnv* env, ERL_NIF_TERM arg, int* index)
{
    if (arg == ATOM(custom1))      *index = -1;
    else if (arg == ATOM(custom2)) *index = -2;
    else if (arg == ATOM(custom3)) *index = -3;
    else if (arg == ATOM(custom4)) *index = -4;
    else if (!enif_get_int(env, arg, index) || (*index < 0))
	return 0;
    return 1;
}

static int get_mark_flags(ErlNifEnv* env, ERL_NIF_TERM list,
			  unsigned int* mark_flags_ptr,
			  mark_set_t* mark_set_ptr)
{
    ERL_NIF_TERM hd, tl;
    unsigned int flags = 0;
    mark_set_t set;

    memset(&set, 0, sizeof(set));
    
    while(enif_get_list_cell(env, list, &hd, &tl)) {
	int arity;
	const ERL_NIF_TERM* elem;
	int brity;	
	const ERL_NIF_TERM* blem;
	
	if (hd == ATOM(notify))
	    flags |= MARK_NOTIFY;
	else if (hd == ATOM(once))
	    flags |= MARK_ONCE;
	else if (hd == ATOM(stop))
	    flags |= MARK_STOP;
	else if (enif_get_tuple(env, hd, &arity, &elem)) {
	    if ((arity == 2) && (elem[0] == ATOM(label))) {
		flags |= MARK_LABEL;
		if (!enif_get_int(env, elem[1], &set.lbl))
		    return 0;
	    }
	    else if ((arity == 2) && (elem[0] == ATOM(set))) {
		flags |= MARK_SET;
		set.rep = 0;
		set.cnt = 0;
		if (enif_get_int(env, elem[1], &set.pos)) {
		    if (set.pos < 0) return 0;
		}
		else if (enif_get_tuple(env, elem[1], &brity, &blem) &&
			 (brity == 2) && (blem[0] == ATOM(label))) {
		    flags |= MARK_GOTO;
		    if (!enif_get_int(env, blem[1], &set.pos))
			return 0;
		}
		else
		    return 0;
	    }
	    else if ((arity == 3) && (elem[0] == ATOM(repeat))) {
		flags |= MARK_REPEAT;

		if (enif_get_int(env, elem[1], &set.pos)) {
		    if (set.pos < 0) return 0;
		}
		else if (enif_get_tuple(env, elem[1], &brity, &blem) &&
			 (brity == 2) && (blem[0] == ATOM(label))) {
		    flags |= MARK_GOTO;
		    if (!enif_get_int(env, blem[1], &set.pos))
			return 0;
		}
		else
		    return 0;		
		if (!enif_get_int(env, elem[2], &set.rep) || (set.rep < 0))
		    return 0;
		set.cnt = set.rep;
	    }
	    else
		return 0;
	}
	else
	    return 0;	
	list = tl;
    }
    *mark_flags_ptr = flags;
    *mark_set_ptr = set;
    return 1;
}


static ERL_NIF_TERM make_form(ErlNifEnv* env, waveform_t form)
{
    switch(form) {
    case NONE: return ATOM(none);
    case CONST: return ATOM(const);
    case SINE: return ATOM(sine);
    case SQUARE: return ATOM(square);
    case PULSE: return ATOM(pulse);
    case TRIANGLE: return ATOM(triangle);
    case SAW: return ATOM(saw);
    case RANDOM: return ATOM(random);
    case CUSTOM1: return ATOM(custom1);
    case CUSTOM2: return ATOM(custom2);
    case CUSTOM3: return ATOM(custom3);
    case CUSTOM4: return ATOM(custom4);
    default: return ATOM(undefined);
    }
}

static ERL_NIF_TERM make_osc_info(ErlNifEnv* env, int index, osc_t* osc)
{
    ERL_NIF_TERM key[5] =
	{ ATOM(index), ATOM(form), ATOM(level), ATOM(freq), ATOM(phase) };
    ERL_NIF_TERM val[5];
    ERL_NIF_TERM props;

    val[0] = enif_make_int(env, index);
    val[1] = make_form(env, osc->form);
    val[2] = enif_make_double(env, osc->level);
    val[3] = enif_make_double(env, osc->freq);
    val[4] = enif_make_double(env, osc->phase);
    enif_make_map_from_arrays(env,key,val,5,&props);
    return props;
}

static ERL_NIF_TERM make_sample_info(ErlNifEnv* env, sample_buffer_t* sp)
{
    return enif_make_tuple2(env, ATOM(num_samples),
			    enif_make_int(env, sp->num_samples));
}


static ERL_NIF_TERM make_wave_info(ErlNifEnv* env, int index, wave_t* wp)
{
    ERL_NIF_TERM key[4] =
	{ ATOM(index), ATOM(chan), ATOM(samples), ATOM(osc) };
    ERL_NIF_TERM val[4];
    ERL_NIF_TERM props;
    ERL_NIF_TERM osc[MAX_OSC];
    int i, j;

    val[0] = enif_make_int(env, index);
    val[1] = enif_make_int(env, wp->chan);
    val[2] = make_sample_info(env, &wp->s);
    j = 0;
    for (i = 0; i < MAX_OSC; i++) {
	if (wp->o_mask & (1 << i))
	    osc[j++] = make_osc_info(env, i, &wp->osc[i]);
    }
    val[3] = enif_make_list_from_array(env, osc, j);
    enif_make_map_from_arrays(env,key,val,4,&props);
    return props;
}

static ERL_NIF_TERM make_wavedef_info(ErlNifEnv* env, wavedef_t* param)
{
    int i, j;
    ERL_NIF_TERM wave[MAX_WAVES];
    ERL_NIF_TERM cust[MAX_WAVE_FORMS];
    ERL_NIF_TERM key[11] =
	{ ATOM(rate), ATOM(mute), ATOM(state), ATOM(t), ATOM(dt),
	  ATOM(pos),  ATOM(mode), ATOM(envelope), ATOM(wave),
	  ATOM(custom), ATOM(mark) };
    ERL_NIF_TERM val[11];
    ERL_NIF_TERM props;

    val[0] = enif_make_double(env, param->rate);
    val[1] = param->mute ? ATOM(true) : ATOM(false);
    val[2] = param->state ? ATOM(running) : ATOM(stopped);
    val[3] = enif_make_double(env, param->t);
    val[4] = enif_make_double(env, param->dt);
    val[5] = enif_make_int(env, param->pos);
    val[6] = enif_make_int(env, param->mode);
    val[7] = ATOM(undefined);

    j = 0;
    for (i = 0; i < MAX_WAVES; i++) {
	if (param->w_mask & (1 << i)) {
	    wave[j++] = make_wave_info(env, i, &param->w[i]);
	}
    }
    val[8] = enif_make_list_from_array(env, wave, j);

    j = 0;
    for (i = 0; i < MAX_WAVE_FORMS; i++) {
	cust[j++] = make_sample_info(env, &param->custom[i]);
    }    
    val[9] = enif_make_list_from_array(env, cust, j);

    val[10] = ATOM(undefined);  // marks!

    enif_make_map_from_arrays(env,key,val,11,&props);
    return props;    
}

// set wave mask and num_waves
static void update_w_mask(wavedef_t* param, int index)
{
    wave_t* wp = &param->w[index];
    if (wp->o_mask || (wp->s.num_samples>0)) {
	param->w_mask |= (1 << index);
    }
    else {
	param->w_mask &= ~(1 << index);
    }
    param->n_waves = __builtin_popcount(param->w_mask);
}


// mix(Format, Channels, [binary()], [Vol::float()]) -> binary()
static ERL_NIF_TERM nif_mix(ErlNifEnv* env, int argc,
			    const ERL_NIF_TERM argv[])
{
    snd_pcm_format_t format;
    size_t num_channels;
    ERL_NIF_TERM list;
    ERL_NIF_TERM vlist;    
    ERL_NIF_TERM dst_bin;
    void* dst;
    void* voice[MAX_VOICES];
    double volume[MAX_VOICES];    
    ErlNifBinary voice_bin[MAX_VOICES];
    unsigned num_voices;
    unsigned num_volumes;    
    size_t num_frames;
    size_t num_samples;
    size_t frame_size;
    size_t voice_size = 0;
    int i;
    
    if (!get_format(env, argv[0], &format))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[1], &num_channels))
	return enif_make_badarg(env);
    if ((num_channels < 1) || (num_channels > MAX_CHANNELS))
	return enif_make_badarg(env);    
    if (!enif_get_list_length(env, argv[2], &num_voices))
	return enif_make_badarg(env);
    if (!enif_get_list_length(env, argv[3], &num_volumes))
	return enif_make_badarg(env);
    if (num_voices == 0)
	return enif_make_list(env, 0);
    if ((num_voices > MAX_VOICES) || (num_volumes > MAX_VOICES))
	return enif_make_badarg(env);

    list = argv[2];
    vlist = argv[3];
    for (i = 0; i < (int)num_voices; i++) {
	ERL_NIF_TERM hd, tl;
	enif_get_list_cell(env, list, &hd, &tl);
	if (!enif_inspect_binary(env, hd, &voice_bin[i]))
	    return enif_make_badarg(env);
	list = tl;
	voice[i] = voice_bin[i].data;
	if (i == 0)
	    voice_size = voice_bin[i].size;
	else if (voice_bin[i].size != voice_size)
	    return enif_make_badarg(env);
	volume[i] = 1.0;
	if (i < num_volumes) {
	    enif_get_list_cell(env, vlist, &hd, &tl);
	    if (!get_number(env, hd, &volume[i]) ||
		((volume[i] < 0.0) || (volume[i] > 1.0)))
		return enif_make_badarg(env);
	    vlist = tl;
	}
    }

    frame_size = snd_pcm_format_size(format, num_channels);
    num_frames = voice_size / frame_size;

    dst = enif_make_new_binary(env, voice_size, &dst_bin);
    num_samples = num_frames*num_channels;
    mix(format, voice, volume, num_voices, dst, num_samples);

    return dst_bin;
}

// resample(SrcRate, DstRate, Format, Channels, Src::binary()) -> Dst:binary().
static ERL_NIF_TERM nif_resample(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    snd_pcm_format_t format;
    double src_rate;
    double dst_rate;
    size_t frame_size;
    size_t dst_size;
    size_t src_frames;
    size_t dst_frames;
    size_t num_channels;
    double dx;
    ErlNifBinary src;
    ERL_NIF_TERM dst_bin;    
    void* dst;
    
    if (!get_number(env, argv[0], &src_rate) || (src_rate <= 0.0))
	return enif_make_badarg(env);
    if (!get_number(env, argv[1], &dst_rate) || (dst_rate <= 0.0))
	return enif_make_badarg(env);
    if (!get_format(env, argv[2], &format))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[3], &num_channels))
	return enif_make_badarg(env);
    if ((num_channels < 1) || (num_channels > MAX_CHANNELS))
	return enif_make_badarg(env);    
    if (!enif_inspect_binary(env, argv[4], &src))
	return enif_make_badarg(env);
        
    frame_size = snd_pcm_format_size(format, num_channels);
    src_frames = src.size / frame_size;
    dx = dst_rate / src_rate;
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

    frame_size = snd_pcm_format_size(src_format, src_channels);
    num_frames = src.size / frame_size;
    dst_size = num_frames * snd_pcm_format_size(dst_format, dst_channels);

    dst = enif_make_new_binary(env, dst_size, &dst_bin);

    reformat(src_format, src_channels, src.data,
	     dst_format, dst_channels, dst,
	     num_frames);
    return dst_bin;
}

#define MAX_FILTER_N 128
// filter(SrcFormat, DstFormat, Filter::[float()], Src::binary() ->
//      Dst::binary().
static ERL_NIF_TERM nif_filter(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    snd_pcm_format_t src_format;
    snd_pcm_format_t dst_format;
    ErlNifBinary src;
    ERL_NIF_TERM dst_bin;
    void* dst;
    size_t dst_size;
    size_t num_samples;
    ERL_NIF_TERM list, head, tail;
    double filt[MAX_FILTER_N];
    size_t filt_len; // N
    
    if (!get_format(env, argv[0], &src_format))
	return enif_make_badarg(env);
    if (!get_format(env, argv[1], &dst_format))
	return enif_make_badarg(env);
    list = argv[2];
    filt_len = 0;
    while(enif_get_list_cell(env, list, &head, &tail) &&
	  (filt_len < MAX_FILTER_N)) {
	if (!get_number(env, head, &filt[filt_len]))
	    return enif_make_badarg(env);
	list = tail;
	filt_len++;
    }
    if (!enif_is_empty_list(env, list))
	return enif_make_badarg(env);    
    if (!enif_inspect_binary(env, argv[3], &src))
	return enif_make_badarg(env);

    num_samples = src.size / snd_pcm_format_size(src_format, 1);
    dst_size = num_samples * snd_pcm_format_size(dst_format, 1);

    dst = enif_make_new_binary(env, dst_size, &dst_bin);

    filter(src.data, src_format, num_samples,
	   filt, filt_len,
	   dst, dst_format, num_samples);
    return dst_bin;
}

static ERL_NIF_TERM nif_wave_new(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    wavedef_t *param;
    ERL_NIF_TERM term;

    param = enif_alloc_resource(wavedef_r, sizeof(wavedef_t));
    wave_init(param);
    term = enif_make_resource(env, param);
    enif_release_resource(param);
    return term;
}

static ERL_NIF_TERM nif_wave_info(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    wavedef_t *param;
    int index;
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    switch(argc) {
    case 1:
	return make_wavedef_info(env, param);
    case 2:
	if (!enif_get_int(env, argv[1], &index))
	    return enif_make_badarg(env);
	if ((index < 0) || (index >= MAX_WAVES))
	    return enif_make_badarg(env);
	return make_wave_info(env, index, &param->w[index]);
    default:
	return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM nif_wave_clear(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    wavedef_t *param;
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    wave_clear(param);
    return ATOM(ok);
}


// set enelope ADSR params
static ERL_NIF_TERM nif_wave_set_adsr(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    double attack, decay, sustain, release;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[1], &attack))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[2], &decay))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[3], &sustain))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[4], &release))
	return enif_make_badarg(env);
    if (wave_set_adsr(param, attack, decay, sustain, release) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

// set enelope general params
// set_envelope [{D1,M1},...{Dn,Mn}]
// {Di,Mi} = {duration, mode}
//              
static ERL_NIF_TERM nif_wave_set_envelope(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    ERL_NIF_TERM list, head, tail;
    size_t n = 0;
    Float_t duration[MAX_PTE];
    unsigned mode[MAX_PTE];
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    list = argv[1];
    while(enif_get_list_cell(env, list, &head, &tail) && (n < MAX_PTE)) {
	double d;
	unsigned m = LIN;
	unsigned s = 0;

	if (enif_is_tuple(env, head)) {
	    int arity;
	    const ERL_NIF_TERM* elem;
	    enif_get_tuple(env, head, &arity, &elem);
	    switch(arity) {
	    case 2:
		if (elem[1] == ATOM(off))
		    m = 0;
		else if (elem[1] == ATOM(linear))
		    m = LIN;
		else if (elem[2] == ATOM(quadratic))
		    m = QUAD;
		else if (elem[2] == ATOM(sustain))
		    s = SUST;
		// fall 
	    case 1:
		if (!get_number(env, elem[0], &d))
		    return enif_make_badarg(env);
		break;
	    }
	}
	else if (!get_number(env, head, &d))
	    return enif_make_badarg(env);
	duration[n] = d;
	mode[n] = m | s;
	list = tail;
	n++;
    }
    if (!enif_is_empty_list(env, list))
	return enif_make_badarg(env);
    if (wave_set_envelope(param, duration, mode, n) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_wave(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    wave_t* wp;
    int index, j;
    ERL_NIF_TERM list, head, tail;
    unsigned ix_mask = 0;
    size_t ix = 0;
    waveform_t form[MAX_OSC];
    Float_t freq[MAX_OSC];
    Float_t level[MAX_OSC];
    Float_t phase[MAX_OSC];
    unsigned mask[MAX_OSC];
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if ((index < 0) || (index >= MAX_WAVES))
	return enif_make_badarg(env);    
    if (!enif_is_list(env, argv[2]))
	return enif_make_badarg(env);
    list = argv[2];

    while(enif_get_list_cell(env, list, &head, &tail) && (ix < MAX_OSC)) {
	double value;
	ERL_NIF_TERM mapval;
	int n = ix;
	
	if (!enif_is_map(env, head))
	    return enif_make_badarg(env);

	// optionally set index
	if (enif_get_map_value(env, head, ATOM(index), &mapval)) {
	    if (!enif_get_int(env, mapval, &n) || (n < 0) || (n >= MAX_OSC))
		return enif_make_badarg(env);
	}
	ix_mask |= (1 << n);  // mark wave as used
	mask[n] = 0;
	
	if (enif_get_map_value(env, head, ATOM(form), &mapval)) {
	    if (!get_form(env, mapval, &form[n]))
		return enif_make_badarg(env);
	    mask[n] |= FORM_BIT;
	}
	if (enif_get_map_value(env, head, ATOM(freq), &mapval)) {
	    if (!get_number(env, mapval, &value))
		return enif_make_badarg(env);
	    freq[n] = value;
	    mask[n] |= FREQ_BIT;
	}
	if (enif_get_map_value(env, head, ATOM(level), &mapval)) {
	    if (!get_number(env, mapval, &value))
		return enif_make_badarg(env);
	    level[n] = value;
	    mask[n] |= LEVEL_BIT;
	}
	if (enif_get_map_value(env, head, ATOM(phase), &mapval)) {
	    if (!get_number(env, mapval, &value))
		return enif_make_badarg(env);
	    phase[n] = value;
	    mask[n] |= PHASE_BIT;
	}
	list = tail;
	ix++;
    }

    wp = &param->w[index];
    
    for (j = 0; j < MAX_OSC; j++) {
	if ((ix_mask & (1 << j)) != 0) {
	    if (mask[j] & FORM_BIT) {
		if (wave_set_form(param, index, j, form[j]) < 0)
		    return enif_make_badarg(env);
	    }
	    if (mask[j] & FREQ_BIT) {
		if (wave_set_freq(param, index, j, freq[j]) < 0)
		    return enif_make_badarg(env);
	    }
	    if (mask[j] & LEVEL_BIT) {
		if (wave_set_level(param, index, j, level[j]) < 0)
		    return enif_make_badarg(env);
	    }
	    if (mask[j] & PHASE_BIT) {
		if (wave_set_phase(param, index, j, phase[j]) < 0)
		    return enif_make_badarg(env);
	    }
	}
	// check if oscilator is in use and update numbers
	if ((wp->osc[j].form != NONE) && (wp->osc[j].freq > 0.0)) {
	    wp->o_mask |= (1 << j);
	}
	else {
	    wp->o_mask &= ~(1 << j);
	}
    }
    update_w_mask(param, index);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_get_rate(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    Rate_t rate;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (wave_get_rate(param, &rate) < 0)
	return enif_make_badarg(env);
    return enif_make_double(env, rate);
}

static ERL_NIF_TERM nif_wave_set_rate(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    double rate;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!get_number(env, argv[1], &rate))
	return enif_make_badarg(env);
    if (wave_set_rate(param, rate) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_mode(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    unsigned int mode;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (argv[1] == ATOM(sustain))
	mode = SUST;
    else if (argv[1] == ATOM(off))
	mode = 0;
    else
	return enif_make_badarg(env);
    if (wave_set_mode(param, mode) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_mute(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int mute;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!get_boolean(env, argv[1], &mute))
	return enif_make_badarg(env);
    if (wave_set_mute(param, mute) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_state(ErlNifEnv* env, int argc,
				       const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    unsigned int state;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (argv[1] == ATOM(running))
	state = 1;
    else if (argv[1] == ATOM(stopped))
	state = 0;
    else
	return enif_make_badarg(env);
    if (wave_set_state(param, state) < 0)
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
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!get_number(env, argv[1], &value))
	return enif_make_badarg(env);
    if (wave_set_attack(param, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_decay(ErlNifEnv* env, int argc,
				       const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!get_number(env, argv[1], &value))
	return enif_make_badarg(env);
    if (wave_set_decay(param, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_sustain(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!get_number(env, argv[1], &value))
	return enif_make_badarg(env);
    if (wave_set_sustain(param, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_release(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!get_number(env, argv[1], &value))
	return enif_make_badarg(env);
    if (wave_set_release(param, value) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_delay(ErlNifEnv* env, int argc,
				       const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!get_number(env, argv[1], &value))
	return enif_make_badarg(env);
    if (wave_set_delay(param, value) < 0)
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
    if (!get_form(env, argv[2], &form))
	return enif_make_badarg(env);
    if (wave_set_form(param, index, 0, form) < 0)
	return enif_make_badarg(env);
    update_w_mask(param, index);
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
    update_w_mask(param, index);   
    return ATOM(ok);
}

// set channel volume
static ERL_NIF_TERM nif_wave_set_volume(ErlNifEnv* env, int argc,
					const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    double vol;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &vol))
	return enif_make_badarg(env);    
    if (wave_set_volume(param, index, (Float_t) vol) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}


// set envelope levels
static ERL_NIF_TERM nif_wave_set_level(ErlNifEnv* env, int argc,
				       const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index, j;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &j))
	return enif_make_badarg(env);   
    if (!get_number(env, argv[3], &value))
	return enif_make_badarg(env);
    if (wave_set_level(param, index, j, value) < 0)
	return enif_make_badarg(env);
    update_w_mask(param, index);    
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_freq(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index, j;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &j))
	return enif_make_badarg(env);    
    if (!get_number(env, argv[3], &value))
	return enif_make_badarg(env);
    if (wave_set_freq(param, index, j, value) < 0)
	return enif_make_badarg(env);
    update_w_mask(param, index);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_phase(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index, j;
    double value;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &j))
	return enif_make_badarg(env);   
    if (!get_number(env, argv[3], &value))
	return enif_make_badarg(env);
    if (wave_set_phase(param, index, j, value) < 0)
	return enif_make_badarg(env);
    update_w_mask(param, index);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_buffer_mode(ErlNifEnv* env, int argc,
					     const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;
    ERL_NIF_TERM list, head, tail;
    int queue = -1;
    int clear = 0;
    int buffer_size = -1;

    list = argv[2];
    while(enif_get_list_cell(env, list, &head, &tail)) {
	int arity;
	const ERL_NIF_TERM* elem;

	if (!enif_get_tuple(env, head, &arity, &elem) || (arity != 2))
	    return enif_make_badarg(env);
	if (elem[0] == ATOM(queue)) {
	    if (!get_boolean(env, elem[1], &queue))
		return enif_make_badarg(env);
	}
	else if (elem[0] == ATOM(clear)) {
	    if (!get_boolean(env, elem[1], &clear))
		return enif_make_badarg(env);
	}
	else if (elem[0] == ATOM(buffer_size)) {
	    if (!enif_get_int(env, elem[1], &buffer_size) ||
		(buffer_size < 0))
		return enif_make_badarg(env);
	}
	else
	    return enif_make_badarg(env);
	list = tail;
    }
    if (!enif_is_empty_list(env, list))
	return enif_make_badarg(env);    

    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!get_samples_index(env, argv[1], &index))
	return enif_make_badarg(env);
    if ((index < 0) || (index >= MAX_WAVES))
	return enif_make_badarg(env);

    if (queue >= 0)
	sample_buffer_set_mode(&param->w[index].s, queue);
    if (buffer_size >= 0)
	sample_buffer_resize(&param->w[index].s, buffer_size, clear);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_samples(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index, pos, chan;
    double src_rate;
    snd_pcm_format_t src_format;
    size_t src_channels;
    ErlNifBinary src;
    size_t frame_size;
    size_t src_frames;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!get_samples_index(env, argv[1], &index))
	return enif_make_badarg(env);	
    if (!enif_get_int(env, argv[2], &pos))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &chan))
	return enif_make_badarg(env);
    if (!get_number(env, argv[4], &src_rate) || (src_rate <= 0.0))
	return enif_make_badarg(env);
    if (!get_format(env, argv[5], &src_format))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[6], &src_channels))
	return enif_make_badarg(env);
    if ((src_channels < 1) || (src_channels > MAX_CHANNELS))
	return enif_make_badarg(env);    
    if (!enif_inspect_binary(env, argv[7], &src))
	return enif_make_badarg(env);

    frame_size = snd_pcm_format_size(src_format, src_channels);
    src_frames = src.size / frame_size;
    if (wave_write_samples(param, index, pos, chan,
			   src_rate, src_format, src_channels,
			   src.data, src_frames) < 0)
	return enif_make_badarg(env);
    update_w_mask(param, index);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_set_num_samples(ErlNifEnv* env, int argc,
					     const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;    
    size_t n;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!get_samples_index(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[2], &n))
	return enif_make_badarg(env);
    if (wave_set_num_samples(param, index, n) < 0)
	return enif_make_badarg(env);
    update_w_mask(param, index);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_get_num_samples(ErlNifEnv* env, int argc,
					     const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int index;    
    size_t n;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!get_samples_index(env, argv[1], &index))    
	return enif_make_badarg(env);
    if (wave_get_num_samples(param, index, &n) < 0)
	return enif_make_badarg(env);
    return enif_make_ulong(env, (unsigned long) n);
}

static ERL_NIF_TERM nif_wave_get_duration(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    double duration;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (wave_get_duration(param, &duration) < 0)
	return enif_make_badarg(env);
    return enif_make_double(env, duration);
}

static ERL_NIF_TERM nif_wave_get_time(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    double tm;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (wave_get_time(param, &tm) < 0)
	return enif_make_badarg(env);
    return enif_make_double(env, tm);
}


static ERL_NIF_TERM nif_wave_set_pos(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int pos;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &pos))
	return enif_make_badarg(env);
    if (wave_set_pos(param, pos) < 0)
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_wave_get_pos(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int pos;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (wave_get_pos(param, &pos) < 0)
	return enif_make_badarg(env);
    return enif_make_int(env, pos);
}

static ERL_NIF_TERM make_mark_list(ErlNifEnv* env,mark_t* mpl,ERL_NIF_TERM list)
{
    while(mpl) {
	ERL_NIF_TERM event;
	mark_t* mpn = mpl->next;
	event = enif_make_tuple4(env,
				 enif_make_copy(env, mpl->ref),
				 enif_make_pid(env, &mpl->pid),
				 enif_make_int(env, mpl->pos),
				 enif_make_copy(env, mpl->user_data));
	list = enif_make_list_cell(env, event, list);
	if (mpl->flags0 & MARK_FREE)
	    free_mark(mpl);
	mpl = mpn;
    }
    return list;
}

//
// wave(wavedef()|[wavedef()],Format,Channels,NumFrames::non_neg_integer()) ->
//   {[event()],#{ peak => float(), energy => float() },Dst:binary()}
//
// wave(wavedef()|[wavedef()], NumFrames::non_neg_integer(), Dst::wavedef()) ->
//   {[event()],#{ peak => float(), energy => float() },Dst:binary()}
//
static ERL_NIF_TERM nif_wave(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    snd_pcm_format_t format;
    unsigned num_voices;
    wavedef_t* voice[MAX_VOICES];
    size_t num_channels;
    size_t num_frames;
    ERL_NIF_TERM dst_bin;
    ERL_NIF_TERM mark_list = enif_make_list(env, 0);
    ERL_NIF_TERM props;
    double peak = 0.0, energy = 0.0;
    ERL_NIF_TERM prop_keys[2] = { ATOM(peak), ATOM(energy) };
    ERL_NIF_TERM prop_values[2];

    if (enif_get_list_length(env, argv[0], &num_voices)) {
	ERL_NIF_TERM hd, tl, list = argv[0];
	int i;
	if ((num_voices == 0) || (num_voices > MAX_VOICES))
	    return enif_make_badarg(env);
	i = 0;
	while(enif_get_list_cell(env, list, &hd, &tl)) {
	    if (!enif_get_resource(env,hd,wavedef_r,(void **)&voice[i]))
		return enif_make_badarg(env);
	    list = tl;
	    i++;
	}
	if (!enif_is_empty_list(env, list))
	    return enif_make_badarg(env);
    }
    else if (enif_get_resource(env,argv[0],wavedef_r,(void **)&voice[0])) {
	num_voices = 1;
    }
    else
	return enif_make_badarg(env);
    
    if (!get_format(env, argv[1], &format))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[2], &num_channels))
	return enif_make_badarg(env);
    if ((num_channels < 1) || (num_channels > MAX_CHANNELS))
	return enif_make_badarg(env);
    if (!get_size_t(env, argv[3], &num_frames))
	return enif_make_badarg(env);

    if ((voice[0]->state == 0) || (voice[0]->n_waves == 0))
	enif_make_new_binary(env, 0, &dst_bin);
    else {
	mark_t* mpl;
	size_t frame_size = snd_pcm_format_size(format, num_channels);
	size_t size = frame_size*num_frames;
	void* dst = enif_make_new_binary(env, size, &dst_bin);
	float fv0[num_channels*num_frames];
	float fv1[num_channels*num_frames];
	int i;
	// void* fvv[2] = { (void*)fv0, (void*)fv1};
	
	mpl = wave_buffer(voice[0],num_channels,fv0,num_frames,&peak,&energy);
	mark_list = make_mark_list(env, mpl, mark_list);

	for (i = 1; i < num_voices; i++) {
	    double p, e;
	    size_t n;
	    float* fptr0;
	    float* fptr1;
	    mpl = wave_buffer(voice[i],num_channels,fv1,num_frames,&p,&e);
	    peak = fmax(peak, p);
	    energy += e;
	    mark_list = make_mark_list(env, mpl, mark_list);
	    n = num_frames*num_channels;
	    fptr0 = fv0;
	    fptr1 = fv1;
	    // add samples in fv1 to fv0 (fixme SIMD)
	    while(n--) {
		*fptr0 += *fptr1++;
		fptr0++;
	    }
	    // mix(SND_PCM_FORMAT_FLOAT,fvv,NULL,2,(void*)fv0,num_frames*num_channels);
	}
	reformat(SND_PCM_FORMAT_FLOAT, num_channels, (void*) fv0,
		 format, num_channels, dst, num_frames);
    }
    prop_values[0] = enif_make_double(env, peak);
    prop_values[1] = enif_make_double(env, energy);
    enif_make_map_from_arrays(env,prop_keys,prop_values,2, &props);
    return enif_make_tuple3(env, mark_list, props, dst_bin);
}

//-spec mark(W::wavedef(), Pid::pid(), Ref::reference(), Pos::integer(),
//	   Flags::[notify|once|stop|restart (={set,0})
//                 {label, Label::integer} |
//                 {goto, Label::integer()} |
//		   {set,Pos::position()} |
//		   {repeat,Pos::position(),Count::integer()}
//                ],
//	   UserData :: term()) ->
//	  ok.
//  position() :: integer() | {label, integer()}
static ERL_NIF_TERM nif_mark(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    ErlNifPid pid;
    int pos;
    mark_t* mp;
    unsigned int mark_flags;
    mark_set_t mark_set;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_get_local_pid(env, argv[1], &pid))
	return enif_make_badarg(env);
    if (!enif_is_ref(env, argv[2]))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &pos) || (pos < 0))
	return enif_make_badarg(env);
    if (!get_mark_flags(env, argv[4], &mark_flags, &mark_set))
	return enif_make_badarg(env);
    mp = alloc_mark();
    mp->pos = pos;
    mp->pid = pid;
    mp->flags0 = mark_flags; // interpreted flags
    mp->set    = mark_set;   // set/repeat params
    mp->flags = enif_make_copy(mp->env, argv[4]);
    mp->ref = enif_make_copy(mp->env, argv[2]);
    mp->user_data = enif_make_copy(mp->env, argv[5]);
    if (wave_add_mark(param, mp) < 0) {
	free_mark(mp);
	return enif_make_badarg(env);
    }
    return ATOM(ok);
}

static ERL_NIF_TERM nif_unmark(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    mark_t* mp;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (!enif_is_ref(env, argv[1]))
	return enif_make_badarg(env);
    if ((mp = wave_remove_mark(param, argv[1])) != NULL) {
	free_mark(mp);
	return ATOM(ok);
    }
    return enif_make_tuple2(env, ATOM(error), ATOM(enoent));
}

static ERL_NIF_TERM nif_get_marks(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    wavedef_t* param;
    int from, to;
    ERL_NIF_TERM list;
    
    if (!enif_get_resource(env, argv[0], wavedef_r, (void **)&param))
	return enif_make_badarg(env);
    if (argc == 2) {
	int period;
	if (!enif_get_int(env, argv[1], &period) || (period < 0))
	    return enif_make_badarg(env);
	from = (param->t * param->rate);
	to   = from + period - 1;
    }
    else {
	if (!enif_get_int(env, argv[1], &from) || (from < 0))
	    return enif_make_badarg(env);
	if (!enif_get_int(env, argv[2], &to) || (to < 0))
	    return enif_make_badarg(env);
    }
    list = enif_make_list(env, 0);
    if (from <= to) {
	// create notification messages [{pid,pos,flags,user_data}]
	// REVERSED!
	int i = wave_find_mark(param, from);
	while ((i < param->n_marks) && (param->markv[i]->pos <= to)) {
	    ERL_NIF_TERM event;
	    mark_t* mp = param->markv[i];
	    event = enif_make_tuple4(env,
				     enif_make_copy(env, mp->ref),
				     enif_make_pid(env, &mp->pid),
				     enif_make_int(env, mp->pos),
				     enif_make_copy(env, mp->user_data));
	    list = enif_make_list_cell(env, event, list);
	    i++;
	}
    }
    return list;
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
    // waveopts
    LOAD_ATOM(queue);
    LOAD_ATOM(clear);
    LOAD_ATOM(buffer_size);    
    // state
    LOAD_ATOM(running);
    LOAD_ATOM(stopped);
    // mode
    LOAD_ATOM(off);
    LOAD_ATOM(linear);
    LOAD_ATOM(quadratic);
    LOAD_ATOM(sustain);
    // osc parts
    LOAD_ATOM(index);
    LOAD_ATOM(form);
    LOAD_ATOM(freq);
    LOAD_ATOM(level);
    LOAD_ATOM(phase);    
    // form
    LOAD_ATOM(none);
    LOAD_ATOM(const);
    LOAD_ATOM(sine);
    LOAD_ATOM(square);
    LOAD_ATOM(pulse);
    LOAD_ATOM(triangle);
    LOAD_ATOM(saw);
    LOAD_ATOM(custom);
    LOAD_ATOM(custom1);
    LOAD_ATOM(custom2);
    LOAD_ATOM(custom3);
    LOAD_ATOM(custom4);
    LOAD_ATOM(random);    
    // mark flags
    LOAD_ATOM(notify);
    LOAD_ATOM(once);
    LOAD_ATOM(stop);
    LOAD_ATOM(set);
    LOAD_ATOM(repeat);
    LOAD_ATOM(label);
    // wave props
    LOAD_ATOM(marks);
    LOAD_ATOM(peak);
    LOAD_ATOM(energy);
    // info atoms
    LOAD_ATOM(num_samples);    
    LOAD_ATOM(chan);
    LOAD_ATOM(samples);
    LOAD_ATOM(osc);
    LOAD_ATOM(rate);
    LOAD_ATOM(mute);
    LOAD_ATOM(state);
    LOAD_ATOM(t);
    LOAD_ATOM(dt);
    LOAD_ATOM(pos);
    LOAD_ATOM(mode);
    LOAD_ATOM(envelope);
    LOAD_ATOM(wave);
    LOAD_ATOM(mark);    

    // format - use SND_FORMAT_LIST to load all atom formats    
#undef SND_FORMAT
#define SND_FORMAT(atm, form) LOAD_ATOM(atm);
SND_FORMAT_LIST

    return 0;
}

void wave_dtor(ErlNifEnv *env, void *obj)
{
    wavedef_t *param = (wavedef_t *)obj;
    wave_clear(param);
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
