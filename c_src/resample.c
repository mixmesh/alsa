
#include "resample.h"

// resample src samples in src_format into dst samples in dst_format
// use simple skip and replicate resampling
// fixme: add some more methods
void resample(snd_pcm_format_t format, size_t channels, 
	      Rate_t src_rate, void* src,
	      Rate_t dst_rate, void* dst,
	      size_t nframes, size_t dst_frames)
{
    ssize_t frame_size = snd_pcm_format_size(format, channels);
    Float_t dx = dst_rate / src_rate;
    Float_t x = dx;
    void* src1 = src;

    while(nframes--) {
	if (x < 1.0) // skip frame
	    x += dx;
	else { // x>=1.0
	    int n = x; // replicate n frames
	    x = (x-n)+dx;
	    if (n <= dst_frames)
		dst_frames -= n;
	    else {
		n = dst_frames;
	    }
	    while(n--) {
		memcpy(dst, src, frame_size);
		dst = (int8_t*) dst + frame_size;
	    }
	}
	src1 = src;
	src = (int8_t*) src + frame_size;
    }
    while(dst_frames--) {
	memcpy(dst, src1, frame_size);
	dst = (int8_t*) dst + frame_size;
    }
}

    
