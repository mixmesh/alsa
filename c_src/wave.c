//
//  Wave generator
//

#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <alsa/asoundlib.h>

#include "wave.h"
#include "reformat.h"

#include "erl_nif.h"

#ifdef DEBUG
#define DEBUGF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#else
#define DEBUGF(f,a...)
#endif

static inline Float_t lin(Float_t t, Float_t a, Float_t b)
{
    return a + (b-a)*t;
}

// https://en.wikipedia.org/wiki/Lagrange_polynomial
// range t is 0..2 but only 0..1 is used for each segment...
static inline Float_t quad(Float_t t, Float_t a, Float_t b, Float_t c)
{
    Float_t a0 = a;
    Float_t a1 = (-3*a + 4*b - c)/2;
    Float_t a2 = (a-2*b+c)/2;
    return (a2*t + a1)*t + a0;
}

// clamp [-1..1]
static inline Float_t clamp(Float_t x)
{
    if (x > 1.0) return 1.0;
    if (x < -1.0) return -1.0;
    return x;
}

// clamp [0..1]
static inline Float_t clamp01(Float_t x)
{
    if (x > 1.0) return 1.0;
    if (x < 0.0) return 0.0;
    return x;
}

// clamp [0..]
static inline Float_t clamp0(Float_t x)
{
    if (x < 0.0) return 0.0;
    return x;
}

static inline Radian_t clamp2pi(Float_t x)
{
    return FMOD(x, 2*M_PI);
}

// generate n sample into sample buffer

static inline Float_t mix2(Float_t a, Float_t b)
{
    if ((a < 0.0) && (b < 0.0))
	return a + b + a*b;
    else if ((a > 0.0) && (b > 0.0))
	return a + b - a*b;
    return a+b;
}

static void init_estate(envelope_t* ep)
{
    ep->cur = 0;
    ep->tsum = 0.0;
}

// given current time update envelop parameters
static void set_estate(envelope_t* ep, Float_t t)
{
    Float_t dur;
    Float_t tsum = 0;
    int cur = 0;
    int n = ep->n;
    pt_t* pp = ep->p;

    while (n-- && (t > (dur=pp->t))) {
	tsum += dur;
	cur++;
	t -= dur;
	pp++;
    }
    ep->tsum = tsum;
    ep->cur = cur;    
}


// update duration and tsum
static void update_duration(envelope_t* ep)
{
    Float_t duration = 0.0;
    int i = 0;

    while(i < ep->cur)
	duration += ep->p[i++].t;
    ep->tsum = duration;
    while(i < ep->n)
	duration += ep->p[i++].t;
    ep->duration = duration;
}

void default_envelope(wavedef_t* param)
{
    param->e.n         = 1;
    param->e.p[0].t    = (Float_t) (24*60*60);
    param->e.p[0].mode = 0;
    param->num_waves = 1;
    param->w[0].level[0] = 0.9;
    param->w[0].form[0] = NONE;
    param->w[0].level[1] = 0.9;
    param->w[0].form[1] = NONE;
}


void wave_init(wavedef_t* param)
{
    memset(param, 0, sizeof(wavedef_t));

    param->t     = 0.0;
    param->pos   = 0;
    param->state = 0;  // stopped
    param->mute  = 0;

    default_envelope(param);
    init_estate(&param->e);
    update_duration(&param->e);
    
    wave_set_rate(param, DEFAULT_RATE);
    wave_set_mode(param, 0);
}

mark_t* alloc_mark()
{
    mark_t* mp = malloc(sizeof(mark_t));
    memset(mp, 0, sizeof(mark_t));
    mp->env = enif_alloc_env();
    return mp;
}

void free_mark(mark_t* mp)
{
    DEBUGF(stderr, "free_mark(%p) env=%p\r\n", mp,mp->env);
    if (mp->env)
	enif_free_env(mp->env);
    free(mp);
}

void wave_reset(wavedef_t* param)
{
    int i;
    size_t n;

    DEBUGF(stderr, "wave_reset\r\n");

    n = MAX_WAVES;
    param->num_waves = 0;
    for (i = 0; i < n; i++) {
	samples_t* sp = &param->w[i].s;
	free_sample_buffer(sp);
    }
    for (i = 0; i < MAX_WAVE_FORMS; i++) {
	samples_t* sp = &param->custom[i];
	free_sample_buffer(sp);
    }
    n = param->num_marks;
    param->num_marks = 0;
    for (i = 0; i < n; i++) {
	free_mark(param->markv[i]);
	param->markv[i] = NULL;
    }
    default_envelope(param);
    param->e.tsum = 0;
    param->e.cur = 0;
    param->t = 0.0;
}

void wave_clear(wavedef_t* param)
{
    wave_reset(param);
}

// interpolate return interpolated t'
// off:        1  p
// linear:     2  p, p+1
// quadratic:  3  p, p+1, p+2
//
static inline Float_t envelope_step(Float_t t, envelope_t* e, int* p, int* q)
{
    Float_t dur;
    Float_t t0 = t;
    int i = e->cur;
    int n = e->n;

    if ((n == 0) || (t < 0.0) || (t > e->duration)) {
	*p = i;
	*q = OFF;
	return t0;
    }
    t0 -= e->tsum;     // offset t to current segment
    while ((i < n) && (t0 > (dur=e->p[i].t))) {
	e->tsum += dur;
	e->cur++;
	t0 -= dur;
	i++;
    }
    if (dur > 0.0) {
	int m = n - i;
	if ((e->p[i].mode & MODEMASK) == 0) {
	    *p = i;
	    *q = OFF;
	    return t0/dur;
	}
	else if ((e->p[i].mode & MODEMASK) == LIN) {
	    *p = i;
	    *q = LIN;
	    return t0/dur;
	}
	else if ((e->p[i].mode & MODEMASK) == QUAD) {
	    if (m >= 3) {
		*p = i;
		*q = QUAD;
		return t0/dur;
	    }
	    else if ((m >= 2) && (n >= 3)) {
		*p = i-1;
		*q = QUAD;
		return 1.0+t0/dur;
	    }
	    else if ((m >= 2)) {
		*p = i;
		*q = LIN;
		return t0/dur;
	    }
	    else {
		*p = i;
		*q = OFF;
		return 0.0;
	    }
	}
    }
    *p = i;
    *q = OFF;
    return 0.0;    
}

#if 0
// generate envelope factor
// This is designed for one sample at a time, maybe sometimes
// better to fill buffer for each duration to avoid state updates
// for every sample! but harder code for SIMD! (if ever)
Float_t envelope_level(Float_t t, int mode, envelope_t* e)
{
    Float_t dur;
    Float_t t0 = t;
    int i;
    int n = e->n;

    if (n == 0) return 0.0;
    if (t < 0.0) return 0.0;
    if (t > e->duration) return 0.0;
    i = e->cur;        // current segment
    // check sustain mode, note down/sustain pedal
    if ((mode & SUST) && (i < n) && (e->p[i].mode & SUST))
	return e->p[i].y;
    t0 -= e->tsum;     // offset t to current segment
    while ((i < n) && (t0 > (dur=e->p[i].t))) {
	e->tsum += dur;
	e->cur++;
	t0 -= dur;
	i++;
    }
//    DEBUGF(stderr, "t=%f: cur=%d: t'=%f: i=%d dur=%f, tsum=%f\r\n",
//		 t, e->cur, t0, i, dur, e->tsum);
    if (dur > 0.0) {
	int m = n - i;
	if ((m == 1) || ((e->p[i].mode & MODEMASK) == 0)) // single point | off?
	    return e->p[i].y;
	else if ((e->p[i+1].mode & MODEMASK) == LIN)
	    return lin(t0/dur, e->p[i].y, e->p[i+1].y);
	else if ((e->p[i+1].mode & MODEMASK) == QUAD) {
	    if (m >= 3)
		return quad(t0/dur, e->p[i].y, e->p[i+1].y, e->p[i+2].y);
	    else if ((m >= 2) && (n >= 3))
		return quad(1.0+t0/dur, e->p[i-1].y, e->p[i].y, e->p[i+1].y);
	    else if ((m >= 2))
		return lin(t0/dur, e->p[i].y, e->p[i+1].y);
	    else
		return 0.0;
	}
    }
    return 0.0;
}
#endif

// fmod(x,y) == (x - trunc(x / y)*y)
// fmod(t,period) == fmod(t, 1/f) = (t - floor(t*f)*period)
// f*fmod(t,period) = (t*f - floor(t*f))

static inline Float_t frac(Float_t tf)
{
    return tf - floor(tf);
}

static inline Float_t shape(Float_t x,samples_t* sp)
{
    size_t n = sp->num_samples;
    if (n == 0) {
	return 0.0;
    }
    else {
	int i = x*(n-1);
	return sp->samples[i];
    }
}

// period = 1/f
// triangle:
//        t        0.00 < t < 0.25
// y(t)  -t + 0.5  0.25 < t < 0.75
//        t - 1.0  0.75 < t < 1.00
static inline Float_t osc(Float_t x, waveform_t form, Float_t phase,
			  samples_t* custom)
{
    switch(form) {
    case CUSTOM1: return shape(x, &custom[0]);
    case CUSTOM2: return shape(x, &custom[1]);
    case CUSTOM3: return shape(x, &custom[2]);
    case CUSTOM4: return shape(x, &custom[3]);
    case SINE: return SINE(2*M_PI*x + phase);
    case SQUARE: return ((x < 0.5) ? 1.0 : -1.0);
    case PULSE: return ((x < 0.5) ? 1.0 : 0.0);
    case TRIANGLE: return 4*((x <= 0.25) ? x :
			     ((x <= 0.75) ? (-x + 0.5) : (x - 1.0)));
    case SAW: return 2*((x <= 0.5) ? x : (x - 1));
    case CONST: return 1.0;
    case NONE: return 0.0;
    }
    return 0.0;
}

static inline Float_t sample(int p, wave_t* w)
{
    if (p < w->s.num_samples)
	return w->s.samples[p];
    return 0.0;
}

// x = f*FMOD(t, period); //  0 <= x <= 1
//   = f*t - floor(f*t) = frac(t*f)
// get sample in range [-1..1]
Float_t wave_sample(Float_t t, int pos, int mode, envelope_t* e,
		    wave_t* w, samples_t* custom)
{
    int p, q;
    Float_t ti = envelope_step(t, e, &p, &q);
    waveform_t form;
    Float_t freq;
    Float_t phase;
    Float_t level;
    Float_t noice;
    Float_t x, y, y1;

    switch(q) {
    case OFF:
	freq = w->freq[p];
	phase = w->phase[p];
	level = w->level[p];
	form = w->form[p];
	x = frac(t*freq);
	y = osc(x, form, phase, custom);
	if ((y1 = sample(pos,w)))
	    y = mix2(y, y1);
	break;
    case LIN:
	freq = w->freq[p];
	if (freq != w->freq[p+1]) {
	    freq = lin(ti, freq,  w->freq[p+1]);
	}
	phase = w->phase[p];
	if (phase != w->phase[p+1])
	    phase = lin(ti,  phase,  w->phase[p+1]);
	level = w->level[p];
	if (level != w->level[p+1])
	    level = lin(ti,  level,  w->level[p+1]);
	form = w->form[p];
	x = frac(t*freq);
	y = osc(x, form, phase, custom);
	if ((y1 = sample(pos,w))) y = mix2(y, y1);
	if (form != w->form[p+1]) {  // form morph
	    Float_t yt;
	    yt = osc(x, w->form[p+1], phase, custom);
	    if (y1) yt = mix2(yt, y1);
	    y = lin(ti, y, yt);
	}
	break;
    case QUAD:
	freq   = quad(ti, w->freq[p],   w->freq[p+1],   w->freq[p+2]);
	phase  = quad(ti, w->phase[p],  w->phase[p+1],  w->phase[p+2]);
	level  = quad(ti, w->level[p],  w->level[p+1],  w->level[p+2]);
	form = w->form[p];
	x = frac(t*freq);	
	y = osc(x, form, phase, custom);
	if ((y1 = sample(pos,w))) y = mix2(y, y1);
	if ((form != w->form[p+1]) || (form != w->form[p+2])) {  // form morph
	    Float_t yt, ys;
	    yt = osc(x, w->form[p+1], phase, custom);
	    if (y1) yt = mix2(yt, y1);
	    ys = osc(x, w->form[p+2], phase, custom);
	    if (y1) ys = mix2(ys, y1);
	    y = quad(ti, y, yt, ys);
	}
	break;
    }
    if ((noice = w->noice[p]) > 0.0) {
	Float_t r = ((Float_t)rand()/(Float_t)(RAND_MAX));
	y += 2*(0.5 - r)*noice;
	y = clamp(y);
    }
    return y*level;
}

// general set envelop values (must reset set repeat/count after)
int wave_set_envelope(wavedef_t* param, Float_t* duration, unsigned* mode,
		      size_t n)
{
    int j;
    envelope_t* ep;
    
    ep = &param->e;
    for (j = 0; j < n; j++) {
	ep->p[j].t = duration[j];
	ep->p[j].mode = mode[j];
    }
    ep->n = n;
    init_estate(ep);
    update_duration(ep);
    return 0;
}

// setup ADSR  (must reset set repeat/count after)
int wave_set_adsr(wavedef_t* param,
		  Float_t attack, Float_t decay,
		  Float_t sustain, Float_t release)
{
    envelope_t* ep = &param->e;
    ep->p[ATTACK].t = clamp0(attack);
    ep->p[DECAY].t  = clamp0(decay);
    ep->p[SUSTAIN].t = clamp0(sustain);
    ep->p[SUSTAIN].mode |= SUST;
    ep->p[RELEASE].t = clamp0(release);
    ep->p[DELAY].t   = 0.0;
    ep->n = 5;
    init_estate(ep);
    update_duration(ep);
    return 0;
}

int wave_set_num_waves(wavedef_t* param, unsigned int num_waves)
{
    if (num_waves > MAX_WAVES)
	return -1;
    param->num_waves = num_waves;
    return 0;
}

int wave_set_rate(wavedef_t* param, Rate_t rate)
{
    if (rate == 0)
	return -1;
    param->rate = rate;
    param->dt = 1.0/rate;
    return 0;
}

int wave_get_rate(wavedef_t* param, Rate_t* rate_ptr)
{
    *rate_ptr = param->rate;
    return 0;
}

int wave_set_mode(wavedef_t* param, unsigned int mode)
{
    param->mode = mode;
    return 0;
}

int wave_set_mute(wavedef_t* param, unsigned int mute)
{
    param->mute = mute;
    return 0;
}

int wave_set_state(wavedef_t* param, unsigned int state)
{
    param->state = state;
    return 0;    
}

int wave_set_time(wavedef_t* param, Float_t t)
{
    if (t < 0.0) return -1;
    param->pos = param->rate*t;
    param->t = t;
    set_estate(&param->e, t);
    return 0;
}

int wave_get_time(wavedef_t* param, double* time_ptr)
{
    *time_ptr = param->t;
    return 0;
}

// set current sample position
int wave_set_pos(wavedef_t* param, int pos)
{
    Float_t t;
    if (pos < 0) return -1;
    param->pos = pos;
    param->t = t = pos*param->dt;
    set_estate(&param->e, t);
    return 0;
}

// get current sample position
int wave_get_pos(wavedef_t* param, int* pos_ptr)
{
    *pos_ptr = param->pos;
    return 0;
}

int wave_get_duration(wavedef_t* param, double* ptr)
{
    *ptr = param->e.duration;
    return 0;
}

int wave_set_attack(wavedef_t* param, Float_t duration)
{
    param->e.p[ATTACK].t = clamp0(duration);
    update_duration(&param->e);
    return 0;
}

int wave_set_decay(wavedef_t* param, Float_t duration)
{
    param->e.p[DECAY].t = clamp0(duration);
    update_duration(&param->e);    
    return 0;
}

int wave_set_sustain(wavedef_t* param, Float_t duration)
{
    param->e.p[SUSTAIN].t = clamp0(duration);
    param->e.p[SUSTAIN].mode |= SUST;
    update_duration(&param->e);    
    return 0;
}

int wave_set_release(wavedef_t* param, Float_t duration)
{
    param->e.p[RELEASE].t = clamp0(duration);
    update_duration(&param->e);    
    return 0;
}

int wave_set_delay(wavedef_t* param, Float_t duration)
{
    param->e.p[DELAY].t = clamp0(duration);
    update_duration(&param->e);
    return 0;
}

int wave_set_chan(wavedef_t* param, int i, int chan)
{
    if ((i < 0) || (i >= MAX_WAVES)) return -1;
    if ((chan < 0) || (chan >= MAX_WAVES)) return -1;
    param->w[i].chan = chan;
    return 0;
}

int wave_set_form(wavedef_t* param, int i, int j, waveform_t form)
{
    if ((i < 0) || (i >= MAX_WAVES)) return -1;
    if ((j < 0) || (j >= MAX_PTW)) return -1;
    param->w[i].form[j] = form;
    return 0;
}

int wave_set_level(wavedef_t* param, int i, int j, Float_t level)
{
    if ((i < 0) || (i >= MAX_WAVES)) return -1;
    if ((j < 0) || (j >= MAX_PTW)) return -1;
    param->w[i].level[j] = clamp01(level);
    return 0;
}

int wave_set_freq(wavedef_t* param, int i, int j, Float_t f)
{
    if ((i < 0) || (i >= MAX_WAVES)) return -1;
    if ((j < 0) || (j >= MAX_PTW)) return -1;
    param->w[i].freq[j] = f;
    param->w[i].period[j] = (f > 0.0) ? 1/f : 1.0;
    return 0;
}

int wave_set_phase(wavedef_t* param, int i, int j, Float_t phase)
{
    wave_t* wp;    
    if ((i < 0) || (i >= MAX_WAVES)) return -1;
    if ((j < 0) || (j >= MAX_PTW)) return -1;    
    wp = &param->w[i];
    wp->phase[j] = phase;
    return 0;
}
    
int wave_set_noice(wavedef_t* param, int i, int j, Float_t noice)
{
    wave_t* wp;    
    if ((i < 0) || (i >= MAX_WAVES)) return -1;
    if ((j < 0) || (j >= MAX_PTW)) return -1;    
    wp = &param->w[i];
    wp->noice[j] = noice;
    return 0;
}

// i is the wave number i < 0 wave form  i>= 0 samples
// j is the start offset to write samples
// k is the channel selector within src
// 
int wave_set_samples(wavedef_t* param, int i, int j, int k,
		     Rate_t src_rate, snd_pcm_format_t src_format,
		     size_t src_channels,
		     void* src, size_t src_frames)
{
    ssize_t sample_size = snd_pcm_format_size(src_format, 1);
    size_t frame_size = sample_size*src_channels;
    size_t dst_frames;
    size_t size;
    samples_t* sp;
    Float_t dx = param->rate / src_rate;

    if ((i < -MAX_WAVE_FORMS) || (i >= MAX_WAVES)) return -1;
    if ((j < 0) || (j >= MAX_SAMPLE_SIZE)) return -1;
    if ((k < 0) || (k >= src_channels)) return -1;

    if (i < 0)  // -1,-2,-3,-4 ... -MAX_WAVE_FORMS
	sp = &param->custom[-i-1];
    else
	sp = &param->w[i].s;

    dst_frames = dx * src_frames;
    size = j + dst_frames;  // make sure we have this size of sample buffer

    if (size > sp->num_samples)
	resize_sample_buffer(sp, size, 1);
    set_sample_buffer(sp, j, k, dx,
		      sample_size, frame_size,
		      src_format, src, src_frames, dst_frames);
    return 0;
}

// resize sample buffer and reallocate/init if needed
int wave_set_num_samples(wavedef_t* param, int i, size_t num_samples)
{
    samples_t* sp;    
    if ((i < -MAX_WAVE_FORMS) || (i >= MAX_WAVES)) return -1;
    if (i < 0)  // -1,-2,-3,-4 ... -MAX_WAVE_FORMS
	sp = &param->custom[-i-1];
    else
	sp = &param->w[i].s;
    return resize_sample_buffer(sp, num_samples, 1);
}

int wave_get_num_samples(wavedef_t* param, int i, size_t* num_samples)
{
    samples_t* sp;    
    if ((i < -MAX_WAVE_FORMS) || (i >= MAX_WAVES)) return -1;
    if (i < 0)  // -1,-2,-3,-4 ... -MAX_WAVE_FORMS
	sp = &param->custom[-i-1];
    else
	sp = &param->w[i].s;    
    *num_samples = sp->num_samples;
    return 0;
}


int wave_set_def(wavedef_t* param, int i, int j,
		 waveform_t form, Float_t freq,
		 Float_t phase, Float_t noice)
{
    if ((i < 0) || (i >= MAX_WAVES)) return -1;
    if ((j < 0) || (j >= MAX_PTW)) return -1;
    param->w[i].form[j] = form;
    param->w[i].freq[j] = freq;
    param->w[i].period[j] = (freq > 0.0) ? 1/freq : 1.0;
    param->w[i].phase[j] = phase;
    param->w[i].noice[j] = noice;
    return 0;
}

// find leftmost element
int wave_find_mark(wavedef_t* param, int pos)
{
    int l = 0;
    int r = param->num_marks;
    while(l < r) {
	int m = (l+r)/2;
	if (param->markv[m]->pos < pos)
	    l = m+1;
	else
	    r = m;
    }
    return l;
}

int wave_add_mark(wavedef_t* param, mark_t* mp)
{
    int i;
    if ((i = param->num_marks) >= MAX_MARKS)
	return -1;
    // simple swap insert from end, improve this if we insert a bit randomly
    // and we have alto of marks
    while((i > 0) && (mp->pos < param->markv[i-1]->pos)) {
	param->markv[i] = param->markv[i-1];
	i--;
    }
    param->markv[i] = mp;
    param->num_marks++;
    /*
    enif_fprintf(stderr, "add mark: index=%d\r\n", i);
    enif_fprintf(stderr, "  pos=%d\r\n", mp->pos);
    enif_fprintf(stderr, "  flags=%x\r\n", mp->flags0);
    enif_fprintf(stderr, "  set: lbl=%d,pos=%d,cnt=%d,rep=%d\r\n",
		 mp->set.lbl,mp->set.pos,mp->set.cnt,mp->set.rep);
    */
    return 0;
}

// "unlink" mark struct
static void wave_remove_mark_i(wavedef_t* param, int i)
{
    param->num_marks--;
    while(i < param->num_marks) {
	param->markv[i] = param->markv[i+1];
	i++;
    }
    param->markv[i] = NULL;
}

mark_t* wave_remove_mark(wavedef_t* param, ERL_NIF_TERM ref)
{
    int i;
    for (i = 0; i < param->num_marks; i++) {
	mark_t* mp = param->markv[i];
	if (enif_compare(mp->ref, ref) == 0) {
	    DEBUGF(stderr, "remove mark %T (i=%d,n=%ld)\r\n",
		   mp->ref, i, param->num_marks);
	    wave_remove_mark_i(param, i);
	    return mp;
	}
    }
    DEBUGF(stderr, "mark %T not found\r\n", ref);
    return NULL;
}

// return mark index if lbl is found -1 otherwise
static int wave_goto_mark(wavedef_t* param, int lbl)
{
    int i;
    for (i = 0; i < param->num_marks; i++) {
	mark_t* mp = param->markv[i];
	if ((mp->flags0 & MARK_LABEL) && (mp->set.lbl == lbl))
	    return i;
    }
    return -1;
}

// make samples buffer, return list of marks contining notifications data
mark_t* wave_buffer(wavedef_t* param, snd_pcm_format_t format,
		    unsigned int channels, void* dst, size_t n)
{
    Float_t t  = param->t;
    Float_t dt = param->dt;
    int pos = param->pos;
    int pos1;
    Float_t y[MAX_CHANNELS];
    int8_t* ptr = dst;
    ssize_t size = snd_pcm_format_size(format, 1);
    int mode = param->mode;
    envelope_t* ep = &param->e;
    mark_t* mpl = NULL;
    int i;
    int mrk, mrk1;

    for (i = 0; i < MAX_CHANNELS; i++)
	y[i] = 0.0;

    mrk = wave_find_mark(param, pos);
    
    //enif_fprintf(stderr, "wave(t=%f,pos1=%d,pos2=%d,mrk=%d)\r\n",
    //  t, pos, pos+n-1, mrk);
    
    while(n--) {
	if (param->mute || !param->state) {// muted || stopped (silence)
	    snd_pcm_format_set_silence(format, ptr, channels);
	    ptr += (channels*size);
	}
	else {	// do wave sample
	    samples_t* custom = param->custom;  // custom wave forms
	    for (i = 0; i < param->num_waves; i++) {
		Float_t value;
		wave_t* wp = &param->w[i];
		int k = wp->chan;
		if (t < ep->duration) {
		    value = wave_sample(t,pos,mode,ep,wp,custom);
		    y[k] = mix2(y[k], value);  // fixme! improve
		}
	    }
	    for (i = 0; i < channels; i++) {
		write_pcm_float(format, (double) clamp(y[i]), ptr);
		y[i] = 0.0;
		ptr += size;
	    }
	}

	// check marks - fixme loop over pcm generation between marks!?
	pos1 = -1;  // the position may be updated
	mrk1 = -1;  // maybe move mrk as well
	while ((mrk < param->num_marks) && (pos > param->markv[mrk]->pos)) {
	    //enif_fprintf(stderr, "mark-next: %d\r\n", param->markv[mrk]->pos);
	    mrk++;
	}
	while ((pos1 < 0) &&
	       (mrk < param->num_marks) &&
	       (pos == param->markv[mrk]->pos)) {
	    mark_t* mp = param->markv[mrk];
	    unsigned int flags = mp->flags0;
	    //enif_fprintf(stderr, "mark-event: %d, flags=%x\r\n",
	    // param->markv[mrk]->pos, flags);
	    if (flags & MARK_SET) {
		if (flags & MARK_GOTO) {
		    if ((i = wave_goto_mark(param, mp->set.pos)) >= 0) {
			pos1 = param->markv[i]->pos;
			mrk1 = i;
		    }
		}
		else
		    pos1 = mp->set.pos;
	    }
	    if (flags & MARK_REPEAT) {
		if (mp->set.cnt == 0) {
		    mp->set.cnt = mp->set.rep;
		}
		else {
		    if (flags & MARK_GOTO) {
			if ((i = wave_goto_mark(param, mp->set.pos)) >= 0) {
			    pos1 = param->markv[i]->pos;
			    mrk1 = i;
			}
		    }
		    else
			pos1 = mp->set.pos;
		    mp->set.cnt--;
		}
	    }
	    if (flags & MARK_STOP) {
		wave_set_state(param, 0);
	    }
	    if (flags & MARK_NOTIFY) { // add to notification list
		mp->next = mpl;
		mpl = mp;
	    }
	    if (flags & MARK_ONCE) {
		wave_remove_mark_i(param, mrk);
		if (flags & MARK_NOTIFY)
		    mp->flags0 |= MARK_FREE;  // mark for free
		else
		    free_mark(mp);
	    }
	    mrk++;
	}
	if (pos1 >= 0) {
	    wave_set_pos(param, pos1);
	    if (mrk1 >= 0)
		mrk = mrk1;
	    else
		mrk = wave_find_mark(param, pos1);
	    pos = pos1;
	    t = param->t;
	    // enif_fprintf(stderr, "set(t=%f,pos=%d,mrk=%d)\r\n", t, pos, mrk);
	}
	else {
	    pos++;
	    t += dt;
	}
    }
    param->t = t;
    param->pos = pos;
    return mpl;
}
