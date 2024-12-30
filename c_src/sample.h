#ifndef __SAMPLE_H__
#define __SAMPLE_H__

#include <stdint.h>
#include <byteswap.h>
#include <alsa/asoundlib.h>

// fmod(x,y) == (x - trunc(x / y)*y)

#ifdef USE_DOUBLE
typedef double Float_t;
#define FMOD(x,y) fmod((x),(y))
#define FSINE(x) sin((x))
#else
typedef float Float_t;
#define FMOD(x,y) fmodf((x),(y))
#define FSINE(x) sinf((x))
#endif

typedef struct {
    int      head;             // extraction pos (when head >= 0)
    int      tail;             // insertion pos (when tail >= 0)
    size_t   num_samples;      // number of samples
    Float_t* samples;          // raw samples
} sample_buffer_t;

static inline int sample_buffer_is_queue(sample_buffer_t* sp)
{
    return (sp->tail >= 0);
}

static inline Float_t sample_buffer_get_sample(sample_buffer_t* sp, int p)
{
    int q;
    if ((q = sp->head) >= 0) {
	p = q++; // read from head & advance
	sp->head = (q >= sp->num_samples) ? 0 : q;
    }
    else if (p >= sp->num_samples)
	return 0.0;
    return sp->samples[p];
}

extern void sample_buffer_init(sample_buffer_t* sp);
extern void sample_buffer_free(sample_buffer_t* sp);
extern void sample_buffer_set_mode(sample_buffer_t* sp, int queue);
extern int  sample_buffer_resize(sample_buffer_t* sp, size_t n, int clear);
extern void sample_buffer_write(sample_buffer_t* sp, int j, int k, Float_t dx,
				ssize_t sample_size, size_t frame_size,
				snd_pcm_format_t src_format, void* src,
				size_t src_frames, size_t dst_frames);

#endif
