
#include "sample.h"
#include "reformat.h"

void init_sample_buffer(samples_t* sp)
{
    sp->samples = NULL;
    sp->num_samples = 0;
}

void free_sample_buffer(samples_t* sp)
{
    if (sp->samples) {
	free(sp->samples);
	sp->samples = NULL;
    }
    sp->num_samples = 0;
}

// resize sample buffer
int resize_sample_buffer(samples_t* sp, size_t n, int clear)
{
    Float_t* xp;
    size_t m = sp->num_samples;

    if (n < m) { // shrink
	if (n == 0) {
	    if (sp->samples)
		free(sp->samples);
	    xp = NULL;
	}
	else {
	    if ((xp=realloc(sp->samples,n*sizeof(Float_t)))==NULL)
		return -1;
	}
	sp->num_samples = n;
	sp->samples = xp;
    }
    else if (n> m) {
	Float_t* xp;
	if ((xp = realloc(sp->samples,n*sizeof(Float_t))) == NULL)
	    return -1;
	if (clear)
	    memset(&xp[m], 0, (n-m)*sizeof(Float_t));
	sp->num_samples = n;
	sp->samples = xp;
    }
    return 0;
}

void set_sample_buffer(samples_t* sp, int j, int k, Float_t dx,
		       ssize_t sample_size, size_t frame_size,
		       snd_pcm_format_t src_format, void* src,
		       size_t src_frames, size_t dst_frames)
{
    Float_t* xp = &sp->samples[j];
    int8_t* ptr = src;
    Float_t x = dx;

    // load every k'th samples from frames
    k *= sample_size;
    
    while(src_frames--) {
	if (x < 1.0)
	    x += dx;
	else { // x>=1.0
	    int n = x; // replicate n frames
	    double y = read_pcm_float(src_format, ptr+k);

	    x = (x-n)+dx;
	    if (n <= dst_frames)
		dst_frames -= n;
	    else {
		n = dst_frames;
	    }
	    while(n--) {
		*xp++ = y;
	    }
	}
	ptr += frame_size;
    }
}
