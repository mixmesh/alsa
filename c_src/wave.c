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

Float_t envelope_time(envelope_t* e)
{
    return e->duration;
}

// generate envelope factor
// This is designed for one sample at a time, maybe sometimes
// better to fill buffer for each duration to avoid state updates
// for every sample! but harder code for SIMD! (if ever)
Float_t envelope_level(Float_t t, envelope_t* e)
{
    Float_t dur;
    Float_t t0 = t;
    int i;
    int n = e->n;

    if (n == 0) return 0.0;
    if (t < 0.0) return 0.0;
    if (t > e->duration) return 0.0;
    i = e->cur;        // current segment
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
	if (m == 1)
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

Float_t osc_level(Float_t t, Float_t f, wave_t* w)
{
    switch(w->form) {
    case SINE:
	return SINE(2*M_PI*f*t + w->phase);
    case SQUARE: {
	Float_t y = f*FMOD(t, 1.0/f);
	return ((y < 0.5) ? -0.5 : 0.5);
    }
    case TRIANGLE: {
	Float_t y = f*FMOD(t, 1.0/f);
	return ((y <= 0.5) ? (4*y - 1.0) : (1.0 - 4*y));
    }
    case SAW: {
	Float_t y = f*FMOD(t, 1.0/f);
	return (2*y - 1.0);
    }
    case CONST:
	return 1.0;
    }
    return 0.0;
}

// get sample in range [-1..1]
Float_t wave_sample(Float_t t, envelope_t* e, wave_t* w)
{
    Float_t f = w->f1;  // fixme!
    Float_t y = osc_level(t, f, w);
    Float_t l = envelope_level(t, e);
    if (w->noice > 0.0) {
	Float_t r = ((Float_t)rand()/(Float_t)(RAND_MAX));
	y += 2*(0.5 - r)*w->noice;
	y = clamp(y);
    }
    return y*l;
}

// update duration and tsum
static void update_env_duration(envelope_t* ep)
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

int wave_set_envelope(wavedef_t* param, int i,
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
    ep->p[RELEASE].t = clamp0(release);
    ep->p[DELAY].t   = 0.0;
    ep->n = 5;       
    ep->repeat = 0;
    ep->count  = 0;
    // reset fields every restart
    ep->cur    = 0;
    ep->tsum   = 0.0;
    update_env_duration(ep);
    return 0;
}

int wave_set_level(wavedef_t* param, int i, Float_t low, Float_t peek,
		   Float_t sust)
{
    envelope_t* ep;

    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    ep = &param->e[i];
    ep->p[ATTACK].y  = clamp01(low);
    ep->p[ATTACK].mode = LIN;
    ep->p[DECAY].y   = clamp01(peek);
    ep->p[DECAY].mode = LIN;
    ep->p[SUSTAIN].y = clamp01(sust);
    ep->p[SUSTAIN].mode = LIN | SUST;
    ep->p[RELEASE].y = clamp01(sust);
    ep->p[RELEASE].mode = LIN;
    ep->p[DELAY].y   = clamp01(low);
    ep->p[DELAY].mode = LIN;
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

int wave_set_low(wavedef_t* param, int i, Float_t level)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->e[i].p[ATTACK].y = clamp01(level);
    param->e[i].p[ATTACK].mode = LIN;    
    param->e[i].p[DELAY].y = clamp01(level);
    param->e[i].p[DELAY].mode = LIN;
    return 0;
}

int wave_set_peek(wavedef_t* param, int i, Float_t level)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->e[i].p[DECAY].y = clamp01(level);
    param->e[i].p[DECAY].mode = LIN;
    return 0;    
}

int wave_set_sust(wavedef_t* param, int i, Float_t level)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->e[i].p[SUSTAIN].y = clamp01(level);
    param->e[i].p[SUSTAIN].mode = LIN|SUST;    
    param->e[i].p[RELEASE].y = clamp01(level);
    param->e[i].p[RELEASE].mode = LIN;    
    return 0;    
}

int  wave_set_wave(wavedef_t* param, int i, waveform_t form,
		   Float_t f1, Float_t f2, Float_t f3,
		   Float_t f4, Float_t f5, Float_t phase,
		   Float_t noice)
{
    wave_t* wp;
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    wp = &param->w[i];
    wp->form = form;
    wp->env = 0;
    wp->chan = 0;
    wp->f1 = f1;
    wp->f2 = f2;
    wp->f3 = f3;
    wp->f4 = f4;
    wp->f5 = f5;
    wp->phase = clamp2pi(phase);
    wp->noice = noice;
    return 0;
}

int wave_set_form(wavedef_t* param, int i, waveform_t form)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->w[i].form = form;
    return 0;
}

int wave_set_env(wavedef_t* param, int i, int env)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    if ((env < 0) || (env >= MAX_WAVES))
	return -1;
    param->w[i].env = env;
    return 0;
}

int wave_set_chan(wavedef_t* param, int i, int chan)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    if ((chan < 0) || (chan >= MAX_WAVES))
	return -1;
    param->w[i].chan = chan;
    return 0;
}

int wave_set_f1(wavedef_t* param, int i, Float_t f1)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->w[i].f1 = f1;
    return 0;
}


int wave_set_f2(wavedef_t* param, int i, Float_t f2)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->w[i].f2 = f2;
    return 0;
}
    
int wave_set_f3(wavedef_t* param, int i, Float_t f3)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->w[i].f3 = f3;
    return 0;
}
    
int wave_set_f4(wavedef_t* param, int i, Float_t f4)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->w[i].f4 = f4;
    return 0;
}
    
int wave_set_f5(wavedef_t* param, int i, Float_t f5)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->w[i].f5 = f5;
    return 0;
}
    
int wave_set_phase(wavedef_t* param, int i, Float_t phase)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->w[i].phase = clamp2pi(phase);
    return 0;
}
    
int wave_set_noice(wavedef_t* param, int i, Float_t noice)
{
    if ((i < 0) || (i >= MAX_WAVES))
	return -1;
    param->w[i].noice = clamp01(noice);
    return 0;
}

void wave_init(wavedef_t* param)
{
    int i;
    param->n = 1;
    param->t = 0.0;
    for ( i = 0; i < MAX_WAVES; i++ ) {
	int j;
	for (j = 0; j < MAX_PT; j++) {
	    param->e[i].p[j].t = 0.5;
	    param->e[i].p[j].y = 0.5;
	    param->e[i].p[j].mode = LIN;
	}
    }
    for ( i = 0; i < MAX_WAVES; i++ ) {
	param->w[i].form = SINE;
	param->w[i].env = 0;
	param->w[i].chan = 0;
	param->w[i].f1 = 440.0;
	param->w[i].f2 = 440.0;
	param->w[i].f3 = 440.0;
	param->w[i].f4 = 440.0;
	param->w[i].f5 = 440.0;
	param->w[i].phase = 0.0;
	param->w[i].noice = 0.0;
    }
    wave_set_rate(param, DEFAULT_RATE);
}

// generate n sample into sample buffer

void wave_buffer(wavedef_t* param, snd_pcm_format_t format, unsigned int channels, void* dst, size_t n)
{
    Float_t y[MAX_WAVES];    
    Float_t dt = param->dt;
    Float_t t  = param->t;
    int8_t* ptr = dst;
    ssize_t size = snd_pcm_format_size(format, 1);

    memset(y, 0, sizeof(y));

    enif_fprintf(stderr, "t0=%f, dt=%f, n=%d\r\n", t, dt, (int)n);
    
    while(n--) {
	int i;
	for (i = 0; i < param->n; i++) {
	    wave_t* wp = &param->w[i];
	    envelope_t* ep = &param->e[wp->env];
	    int k = wp->chan;
	    y[k] += wave_sample(t, ep, wp);
	}
	for (i = 0; i < channels; i++) {
	    write_pcm_float(format, (double) clamp(y[i]), ptr);
	    y[i] = 0.0;
	    ptr += size;
	}
	t += dt;
    }

    enif_fprintf(stderr, "t1=%f, nn=%d\r\n", t,
		 (ptr - (int8_t*)dst)/size);
    param->t = t;
}
