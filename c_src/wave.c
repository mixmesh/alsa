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

#if 0
static Float_t envelope_time(envelope_t* e)
{
    return e->duration;
}
#endif

// interpolate return interpolated t'
// off:        1  p
// linear:     2  p, p+1
// quadratic:  3  p, p+1, p+2
//
static inline Float_t envelope_step(Float_t t, envelope_t* e, int* p, int* q)
{
    Float_t dur;
    Float_t t0 = t;
    int i = e->s.cur;
    int n = e->n;

    if ((n == 0) || (t < 0.0) || (t > e->duration)) {
	*p = i;
	*q = OFF;
	return t0;
    }
    t0 -= e->s.tsum;     // offset t to current segment
    while ((i < n) && (t0 > (dur=e->p[i].t))) {
	e->s.tsum += dur;
	e->s.cur++;
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
//    enif_fprintf(stderr, "t=%f: cur=%d: t'=%f: i=%d dur=%f, tsum=%f\r\n",
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

static inline Float_t osc(Float_t t, Float_t f, Float_t period,
			  waveform_t form, Float_t phase)
{
    switch(form) {
    case SINE: // optimise!
	return SINE(2*M_PI*f*t + phase);

    case SQUARE: {
	Float_t x = f*FMOD(t, period);
	return ((x < 0.5) ? 1.0 : -1.0);
    }
    case PULSE: {
	Float_t x = f*FMOD(t, period);
	return ((x < 0.5) ? 1.0 : 0.0);
    }
    //        t        0.00 < t < 0.25
    // y(t)  -t + 0.5  0.25 < t < 0.75
    //        t - 1.0  0.75 < t < 1.00
    case TRIANGLE: {
	Float_t x = f*FMOD(t, period);
	return 4*((x <= 0.25) ? x :
		  ((x <= 0.75) ? (-x + 0.5) : (x - 1.0)));
    }
    case SAW: {
	Float_t x = f*FMOD(t, period);
	return 2*((x <= 0.5) ? x : (x - 1));
    }
    case CONST:
	return 1.0;
    }
    return 0.0;
}

// get sample in range [-1..1]
Float_t wave_sample(Float_t t, int mode, envelope_t* e, wave_t* w)
{
    int p, q;
    Float_t ti = envelope_step(t, e, &p, &q);
    waveform_t form;
    Float_t freq;
    Float_t period;
    Float_t phase;
    Float_t level;
    Float_t noice;
    Float_t y;

    switch(q) {
    case OFF:
	noice  = w->noice[p];
	freq   = w->freq[p];
	period = w->period[p];
	phase  = w->phase[p];
	level  = w->level[p];
	form   = w->form[p];
	y = osc(t, freq, period, form, phase);
	break;
    case LIN:
	noice  = w->noice[p];
	freq   = w->freq[p];
	period = w->period[p];
	if (freq != w->freq[p+1]) {
	    freq = lin(ti, freq,  w->freq[p+1]);
	    period = lin(ti, period, w->period[p+1]); // or calc?
	}
	phase = w->phase[p];
	if (phase != w->phase[p+1])
	    phase = lin(ti,  phase,  w->phase[p+1]);
	level = w->level[p];
	if (level != w->level[p+1])
	    level = lin(ti,  level,  w->level[p+1]);
	form = w->form[p];
	y = osc(t, freq, period, form, phase);
	if (form != w->form[p+1]) {  // form morph
	    Float_t y1;
	    y1 = osc(t, freq, period, w->form[p+1], phase);
	    y = lin(ti, y, y1);
	}
	break;
    case QUAD:
	noice = w->noice[p];
	freq   = quad(ti, w->freq[p],   w->freq[p+1],   w->freq[p+2]);
	period = quad(ti, w->period[p], w->period[p+1], w->period[p+2]);
	phase  = quad(ti, w->phase[p],  w->phase[p+1],  w->phase[p+2]);
	level  = quad(ti, w->level[p],  w->level[p+1],  w->level[p+2]);
	form = w->form[p];
	y = osc(t, freq, period, form, phase);
	if ((form != w->form[p+1]) || (form != w->form[p+2])) {  // form morph
	    Float_t y1, y2;
	    y1 = osc(t, freq, period, w->form[p+1], phase);
	    y2 = osc(t, freq, period, w->form[p+2], phase);
	    y = quad(ti, y, y1, y2);
	}
	break;
    }
    // enif_fprintf(stderr, "t=%f,ti=%f,p=%d,q=%d,y=%f,l=%f\r\n", t, ti, p, q, y,level);
    
    if (noice > 0.0) {
	Float_t r = ((Float_t)rand()/(Float_t)(RAND_MAX));
	y += 2*(0.5 - r)*noice;
	y = clamp(y);
    }
    return y*level;
}

// update duration and tsum
static void update_env_duration(envelope_t* ep)
{
    Float_t duration = 0.0;
    int i = 0;

    while(i < ep->s.cur)
	duration += ep->p[i++].t;
    ep->s.tsum = duration;
    while(i < ep->n)
	duration += ep->p[i++].t;
    ep->duration = duration;
}

static void restart_estate(envelope_t* ep)
{
    ep->s.cur = 0;
    ep->s.tsum = 0.0;
}

static void reset_estate(envelope_t* ep)
{
    ep->s.count = ep->repeat;
    restart_estate(ep);
}

// general set envelop values (must reset set repeat/count after)
int wave_set_envelope(wavedef_t* param, int i,
		      Float_t* duration, unsigned* mode,
		      size_t n)
{
    int j;
    envelope_t* ep;
    
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    ep = &param->e[i];
    for (j = 0; j < n; j++) {
	ep->p[j].t = duration[j];
	ep->p[j].mode = mode[j];
    }
    ep->n = n;
    ep->repeat = 0;
    reset_estate(ep);
    update_env_duration(ep);
    return 0;
}

// setup ADSR  (must reset set repeat/count after)
int wave_set_adsr(wavedef_t* param, int i,
		  Float_t attack, Float_t decay,
		  Float_t sustain, Float_t release)
{
    envelope_t* ep;

    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    ep = &param->e[i];
    ep->p[ATTACK].t = clamp0(attack);
    ep->p[DECAY].t  = clamp0(decay);
    ep->p[SUSTAIN].t = clamp0(sustain);
    ep->p[SUSTAIN].mode |= SUST;
    ep->p[RELEASE].t = clamp0(release);
    ep->p[DELAY].t   = 0.0;
    ep->n = 5;
    ep->repeat = 0;
    // reset fields every restart
    reset_estate(ep);
    update_env_duration(ep);
    return 0;
}

int wave_set_nwaves(wavedef_t* param, unsigned int n)
{
    if (n > MAX_WAVES)
	return -1;
    param->n = n;
    return 0;
}

int wave_set_rate(wavedef_t* param, unsigned int rate)
{
    if (rate == 0)
	return -1;
    param->rate = rate;
    param->dt = 1.0/((Float_t) rate);
    return 0;
}

int wave_set_mode(wavedef_t* param, unsigned int mode)
{
    param->mode = mode;
    return 0;
}

int wave_set_time(wavedef_t* param, Float_t t)
{
    if (t < 0.0)
	return -1;
    param->t = t;
    // FIXME update cur and tsum!
    return 0;
}

int wave_set_attack(wavedef_t* param, int i, Float_t duration)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->e[i].p[ATTACK].t = clamp0(duration);
    update_env_duration(&param->e[i]);
    return 0;
}

int wave_set_decay(wavedef_t* param, int i, Float_t duration)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->e[i].p[DECAY].t = clamp0(duration);
    update_env_duration(&param->e[i]);    
    return 0;
}

int wave_set_sustain(wavedef_t* param, int i,Float_t duration)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->e[i].p[SUSTAIN].t = clamp0(duration);
    param->e[i].p[SUSTAIN].mode |= SUST;
    update_env_duration(&param->e[i]);    
    return 0;
}

int wave_set_release(wavedef_t* param, int i, Float_t duration)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->e[i].p[RELEASE].t = clamp0(duration);
    update_env_duration(&param->e[i]);    
    return 0;
}

int wave_set_delay(wavedef_t* param, int i, Float_t duration)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->e[i].p[DELAY].t = clamp0(duration);
    update_env_duration(&param->e[i]);
    return 0;
}

int wave_set_env(wavedef_t* param, int i, int env)
{
    if ((i < 0) || (i >= MAX_WAVES)) return -1;
    if ((env < 0) || (env >= MAX_WAVES)) return -1;
    param->w[i].env = env;
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

void wave_init(wavedef_t* param)
{
    int i;
    param->n = 1;
    param->t = 0.0;
    for ( i = 0; i < MAX_WAVES; i++ ) {
	int j;
	for (j = 0; j < MAX_PTE; j++) {
	    param->e[i].p[j].t = 0.5;
	    param->e[i].p[j].mode = LIN;
	}
    }
    for ( i = 0; i < MAX_WAVES; i++ ) {
	int j;
	param->w[i].env = 0;
	param->w[i].chan = 0;
	for (j = 0; j < MAX_PTW; j++) {
	    param->w[i].level[j] = 0.5;
	    param->w[i].form[j] = SINE;
	    param->w[i].freq[j] = 440.0;
	    param->w[i].period[j] = 1/440.0;
	    param->w[i].phase[j] = 0.0;
	    param->w[i].noice[j] = 0.0;
	}
    }
    wave_set_rate(param, DEFAULT_RATE);
    wave_set_mode(param, 0);
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

void wave_buffer(wavedef_t* param, snd_pcm_format_t format, unsigned int channels, void* dst, size_t n)
{
    Float_t y[MAX_WAVES];
    Float_t dt = param->dt;
    Float_t t  = param->t;
    int8_t* ptr = dst;
    ssize_t size = snd_pcm_format_size(format, 1);
    int mode = param->mode;
    
    memset(y, 0, sizeof(y));

    // enif_fprintf(stderr, "t0=%f, dt=%f, n=%d\r\n", t, dt, (int)n);
    
    while(n--) {
	int i;
	for (i = 0; i < param->n; i++) {
	    Float_t value;
	    wave_t* wp = &param->w[i];
	    envelope_t* ep = &param->e[wp->env];
	    int k = wp->chan;
	    // fixme advance all 
	    value = wave_sample(t, mode, ep, wp);
	    y[k] = mix2(y[k], value);  // fixme! improve
	}
	for (i = 0; i < channels; i++) {
	    write_pcm_float(format, (double) clamp(y[i]), ptr);
	    y[i] = 0.0;
	    ptr += size;
	}
	t += dt;
    }

//    enif_fprintf(stderr, "t1=%f, nn=%d\r\n", t,
//		 (ptr - (int8_t*)dst)/size);
    param->t = t;
}
