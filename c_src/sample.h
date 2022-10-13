#ifndef __SAMPLE_H__
#define __SAMPLE_H__

#include <stdint.h>
#include <byteswap.h>
#include <alsa/asoundlib.h>

// fmod(x,y) == (x - trunc(x / y)*y)

#ifdef USE_DOUBLE
typedef double Float_t;
#define FMOD(x,y) fmod((x),(y))
#define SINE(x) sin((x))
#else
typedef float Float_t;
#define FMOD(x,y) fmodf((x),(y))
#define SINE(x) sinf((x))
#endif

typedef struct {
    size_t   num_samples;      // number of samples
    Float_t* samples;          // raw samples
} samples_t;

extern int resize_sample_buffer(samples_t* sp, size_t n, int clear);

extern void free_sample_buffer(samples_t* sp);

extern void set_sample_buffer(samples_t* sp, int j, int k, Float_t dx,
			      ssize_t sample_size, size_t frame_size,
			      snd_pcm_format_t src_format, void* src,
			      size_t src_frames, size_t dst_frames);

#endif

    
