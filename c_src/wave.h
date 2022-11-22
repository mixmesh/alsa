#ifndef __WAVE_H__
#define __WAVE_H__

#include <stdint.h>
#include <alsa/asoundlib.h>

#include "erl_nif.h"

#define DEFAULT_RATE 16000.0
#define MAX_CHANNELS 8
#define MAX_WAVES 8
#define MAX_WAVE_FORMS 4
// #define USE_DOUBLE
#define MAX_SAMPLE_SIZE 158760000  // 1 hour in 44.1kHz

#include "sample.h"

#define MAX_PTE 15
#define MAX_OSC (MAX_PTE+1)

// index in envelop.p[]  "standard" envelope parameters
#define ATTACK  0
#define DECAY   1
#define SUSTAIN 2
#define RELEASE 3
#define DELAY   4

// flags
#define MODEMASK 0x3
#define OFF      0
#define LIN      1
#define QUAD     2
#define SUST     4  // this point can be as sustain

typedef struct {
    Float_t t;
    unsigned mode;
} pt_t;

typedef struct {
    int n;                // number of points in pt 0..MAX_PT
    Float_t duration;     // sum(i=0..n,p[i].t)
    int      cur;         // current point in pt
    Float_t  tsum;        // sum(i=0..pt, p[i].t)
    pt_t p[MAX_PTE];      // point/level/flag (fixme dynamic..?)
} envelope_t;    

typedef enum {
    NONE,
    CONST,
    SINE,
    SQUARE,
    PULSE,
    TRIANGLE,
    SAW,
    RANDOM,
    CUSTOM1,
    CUSTOM2,
    CUSTOM3,
    CUSTOM4
} waveform_t;

typedef float Radian_t;  // 0.0 - 2*3.14
typedef float Rate_t;    // Hz

typedef struct {
    waveform_t form;  // form to use
    Float_t  level;   // volume
    Float_t  freq;    // frequency
    Float_t  period;  // calculated 1/freq[i]
    Radian_t phase;   // phase shift
} osc_t;

typedef struct {
    int chan;                  // output audio channel
    unsigned int o_mask;       // oscillators defined mask
    sample_buffer_t s;               // samples
    osc_t    osc[MAX_OSC];
} wave_t;

// flag0
#define MARK_NOTIFY  0x01  // send notification
#define MARK_ONCE    0x02  // trigger once, auto removed
#define MARK_STOP    0x04  // stop marker
#define MARK_SET     0x08  // change position {set,Pos}
#define MARK_REPEAT  0x10  // maybe change pos and maybe reset counter
#define MARK_LABEL   0x20  // mark has a label
#define MARK_GOTO    0x40  // mark use GOTO label instead of position
#define MARK_FREE    0x80  // free needed
// cnt=0 => no repeat (rep must be 0)
// cnt=1 => cnt=rep, continue
// cnt=n => cnt=n-1, take jump
typedef struct
{
    int lbl;   // mark label
    int pos;   // next position SET/REPEAT | label if GOTO
    int rep;   // repeat value (config)
    int cnt;   // current count (reset to rep)
} mark_set_t;

typedef struct _mark_t
{
    struct _mark_t* next;
    ErlNifPid pid;
    ERL_NIF_TERM ref;
    int pos;
    unsigned int flags0;
    mark_set_t set;
    ERL_NIF_TERM flags;
    ERL_NIF_TERM user_data;
    ErlNifEnv* env;
} mark_t;

extern mark_t* alloc_mark(void);
extern void free_mark(mark_t* mp);

#define MAX_MARKS 32

typedef struct {
    Rate_t rate;            // sample rate
    int mute;               // bool: mute=1 unmute=0
    unsigned int state;     // running=1 stopped=0
    Float_t t;              // current time
    Float_t dt;             // time step
    int     pos;            // current sample position
    int     mode;           // SUST (pedal)
    envelope_t e;           // the envelope
    unsigned int n_waves;   // number of active wave forms
    unsigned int w_mask;    // active waves mask
    wave_t w[MAX_WAVES];
    sample_buffer_t custom[MAX_WAVE_FORMS];
    size_t n_marks;
    mark_t* markv[MAX_MARKS];
} wavedef_t;

extern void wave_init(wavedef_t* param);
extern void wave_reset(wavedef_t* param);
extern void wave_clear(wavedef_t* param);

// envelope access
extern int wave_set_envelope(wavedef_t* param,
			     Float_t* duration, unsigned* mode, size_t n);
extern int wave_set_adsr(wavedef_t* param,
			 Float_t attack, Float_t decay,
			 Float_t sustain, Float_t release);
extern int wave_set_rate(wavedef_t* param, Rate_t rate);
extern int wave_get_rate(wavedef_t* param, Rate_t* rate_ptr);
extern int wave_set_mode(wavedef_t* param, unsigned int mode);
extern int wave_set_mute(wavedef_t* param, int mute);
extern int wave_set_state(wavedef_t* param, unsigned int state);
extern int wave_set_time(wavedef_t* param, Float_t t);
extern int wave_get_time(wavedef_t* param, double* time);
extern int wave_set_pos(wavedef_t* param, int pos);
extern int wave_get_pos(wavedef_t* param, int* pos_ptr);
extern int wave_set_attack(wavedef_t* param, Float_t duration);
extern int wave_set_decay(wavedef_t* param, Float_t duration);
extern int wave_set_sustain(wavedef_t* param, Float_t duration);
extern int wave_set_release(wavedef_t* param, Float_t duration);
extern int wave_set_delay(wavedef_t* param, Float_t duration);
extern int wave_set_chan(wavedef_t* param, int i, int chan);
extern int wave_set_def(wavedef_t* param, int i, int j,
			waveform_t form, Float_t freq,Float_t phase);

extern int wave_set_form(wavedef_t* param, int i, int j, waveform_t form);
extern int wave_set_level(wavedef_t* param, int i, int j, Float_t l);
extern int wave_set_freq(wavedef_t* param, int i, int j, Float_t f);
extern int wave_set_phase(wavedef_t* param, int i, int j, Float_t phase);
extern int wave_write_samples(wavedef_t* param, int i, int j, int k,
			      Rate_t src_rate, snd_pcm_format_t src_format,
			      size_t src_channels,
			      void* src, size_t src_frames);
extern int wave_set_num_samples(wavedef_t* param, int i, size_t num_samples);
extern int wave_get_num_samples(wavedef_t* param, int i, size_t* num_samples);
extern int wave_get_duration(wavedef_t* param, double* duration);

extern int wave_find_mark(wavedef_t* param, int pos);
extern int wave_add_mark(wavedef_t* param, mark_t* mp);
extern mark_t* wave_remove_mark(wavedef_t* param, ERL_NIF_TERM ref);

extern mark_t* wave_buffer(wavedef_t* param, snd_pcm_format_t format, unsigned int channels, void* dst, size_t n, double* peek_ptr, double* energy_ptr);

#endif
