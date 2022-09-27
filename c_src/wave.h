#ifndef __WAVE_H__
#define __WAVE_H__

#include <stdint.h>
#include <alsa/asoundlib.h>

#define DEFAULT_RATE 16000
#define MAX_WAVES 8
// #define USE_DOUBLE

#ifdef USE_DOUBLE
typedef double Float_t;
#define FMOD(x,y) fmod((x),(y))
#define SINE(x) sin((x))
#else
typedef float Float_t;
#define FMOD(x,y) fmodf((x),(y))
#define SINE(x) sinf((x))
#endif

#define MAX_PT 10

// index in envelop.p[]
#define ATTACK  0
#define DECAY   1
#define SUSTAIN 2
#define RELEASE 3
#define DELAY   4

// flags
#define MODEMASK 0x3
#define LIN      1
#define QUAD     2
#define SUST     4  // this point can be as sustain

typedef struct {
    Float_t t;
    Float_t y;
    unsigned mode;
} pt_t;

typedef struct {
    int n;                // number of points in pt 0..MAX_PT
    Float_t duration;     // sum(i=0..n,p[i].t)
    pt_t p[MAX_PT];       // point/level/flag
    unsigned int repeat;  // number of repeats
    // state
    unsigned count;       // repeat count
    int      cur;         // current point in pt
    Float_t  tsum;        // sum(i=0..pt, p[i].t)
} envelope_t;    

typedef enum {
    CONST,
    SINE,
    SQUARE,
    TRIANGLE,
    SAW
} waveform_t;

typedef Float_t Radian_t;  // 0.0 - 3.14

typedef struct {
    waveform_t form;
    int env;        // evelope index to use
    int chan;       // wave channel to use
    Float_t f1;     // zero -> peek   : f1 -> f2
    Float_t f2;     // peek -> sust   : f2 -> f3
    Float_t f3;     // sust1 -> sust2 : f3 -> f4
    Float_t f4;     // sust2 -> zero  : f4 -> f5
    Float_t f5;     // 
    Radian_t phase;  // phase shift
    Float_t noice;  // add some random noice
} wave_t;

typedef struct {
    unsigned int rate;       // sample rate
    unsigned int n;          // number of wave forms
    Float_t t;               // current time
    Float_t dt;              // time step
    envelope_t e[MAX_WAVES];
    wave_t w[MAX_WAVES];
} wavedef_t;

extern void wave_init(wavedef_t* param);

// envelope access
extern int  wave_set_envelope(wavedef_t* param, int i,
			      Float_t attack, Float_t decay,
			      Float_t sustain, Float_t release);
extern int wave_set_nwaves(wavedef_t* param, unsigned int n);
extern int wave_set_rate(wavedef_t* param, unsigned int rate);
extern int wave_set_time(wavedef_t* param, Float_t t);
extern int wave_set_attack(wavedef_t* param, int i, Float_t duration);
extern int wave_set_decay(wavedef_t* param, int i, Float_t duration);
extern int wave_set_sustain(wavedef_t* param, int i,Float_t duration);
extern int wave_set_release(wavedef_t* param, int i, Float_t duration);
extern int wave_set_level(wavedef_t* param, int i, Float_t low, Float_t peek,
			  Float_t sust);
extern int wave_set_low(wavedef_t* param, int i, Float_t level);
extern int wave_set_peek(wavedef_t* param, int i, Float_t level);
extern int wave_set_sust(wavedef_t* param, int i, Float_t level);

// wave form
extern int  wave_set_wave(wavedef_t* param, int i, waveform_t form,
			  Float_t f1, Float_t f2, Float_t f3,
			  Float_t f4, Float_t f5, Float_t phase,
			  Float_t noice);
extern int wave_set_form(wavedef_t* param, int i, waveform_t form);
extern int wave_set_env(wavedef_t* param, int i, int env);
extern int wave_set_chan(wavedef_t* param, int i, int chan);
extern int wave_set_f1(wavedef_t* param, int i, Float_t f1);
extern int wave_set_f2(wavedef_t* param, int i, Float_t f2);
extern int wave_set_f3(wavedef_t* param, int i, Float_t f3);
extern int wave_set_f4(wavedef_t* param, int i, Float_t f4);
extern int wave_set_f5(wavedef_t* param, int i, Float_t f5);
extern int wave_set_phase(wavedef_t* param, int i, Float_t phase);
extern int wave_set_noice(wavedef_t* param, int i, Float_t noice);

extern void wave_buffer(wavedef_t* param, snd_pcm_format_t format, unsigned int channels, void* dst, size_t n);

#endif
