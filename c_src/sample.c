
#include "sample.h"
#include "reformat.h"

void sample_buffer_init(sample_buffer_t* sp)
{
    sp->head = sp->tail = -1;
    sp->num_samples = 0;
    sp->samples = NULL;
}

void sample_buffer_free(sample_buffer_t* sp)
{
    if (sp->samples) {
	free(sp->samples);
	sp->samples = NULL;
    }
    sp->head = sp->tail = -1; 
    sp->num_samples = 0;
}

void sample_buffer_set_mode(sample_buffer_t* sp, int queue)
{
    if (queue) {
	if ((sp->head == -1) && (sp->tail == -1))
	    sp->head = sp->tail = 0;
    }
    else {
	sp->head = sp->tail = -1;
    }
}

// resize sample buffer
int sample_buffer_resize(sample_buffer_t* sp, size_t n, int and_clear)
{
    Float_t* xp;
    size_t m = sp->num_samples;

    if (n < m) { // shrink
	if (n == 0) {
	    if ((sp->head >= 0) && (sp->tail >= 0)) {
		sp->head = sp->tail = 0;
		return 0;
	    }
	    else {
		if (sp->samples)
		    free(sp->samples);
		sp->samples = NULL;
		sp->num_samples = 0;
		return 0;
	    }
	}
	else {
	    if ((xp=realloc(sp->samples,n*sizeof(Float_t)))==NULL)
		return -1;
	    sp->num_samples = n;
	    sp->samples = xp;
	    // Fixme: move data!!???
	    if ((sp->head >= 0) && (sp->head >= sp->num_samples))
		sp->head %= n;
	    if ((sp->tail >= 0) && (sp->tail >= sp->num_samples))
		sp->tail %= n;
	}
    }
    else if (n > m) {
	Float_t* xp;
	if ((xp = realloc(sp->samples,n*sizeof(Float_t))) == NULL)
	    return -1;
	if (and_clear)
	    memset(&xp[m], 0, (n-m)*sizeof(Float_t));
	sp->num_samples = n;
	sp->samples = xp;
    }
    return 0;
}
//
// j is the position in the buffer
// k is the channel number to write
// 
void sample_buffer_write(sample_buffer_t* sp, int j, int k, Float_t dx,
			 ssize_t sample_size, size_t frame_size,
			 snd_pcm_format_t src_format, void* src,
			 size_t src_frames, size_t dst_frames)
{
    Float_t* xp;
    int8_t* ptr = src;
    Float_t x = dx;
    double y = 0.0;
    int p;

    if (sp->tail < 0)
	p = j;
    else
	p = sp->tail;

    xp = &sp->samples[p];
    
    // load every k'th (channel k) samples from frames
    k *= sample_size;
    
    while(src_frames--) {
	if (x < 1.0)
	    x += dx;
	else { // x>=1.0
	    int n = x; // replicate n frames
	    y = read_pcm_float(src_format, ptr+k);
	    x = (x-n)+dx;
	    if (n <= dst_frames)
		dst_frames -= n;
	    else {
		n = dst_frames;
	    }
	    while(n--) {
		*xp++ = y;
		p++;
		if (p >= sp->num_samples) {
		    if (sp->tail < 0)
			return;
		    p = 0;
		    xp = &sp->samples[0];
		}
	    }
	}
	ptr += frame_size;
    }
    while(dst_frames--) {
	*xp++ = y;
	p++;
	if (p >= sp->num_samples) {
	    if (sp->tail < 0)
		return;
	    p = 0;
	    xp = &sp->samples[0];
	}
    }
    if (sp->tail >= 0)
	sp->tail = p;
}
