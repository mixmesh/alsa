#include <stdint.h>
#include <byteswap.h>
#include <alsa/asoundlib.h>

// resample src samples in src_format into dst samples in dst_format
// use simple skip and replicate resampling
// fixme: add some more methods
void resample(snd_pcm_format_t format, size_t channels, 
	      size_t src_rate, void* src,
	      size_t dst_rate, void* dst,
	      size_t nframes, size_t dst_frames)
{
    ssize_t frame_size = snd_pcm_format_size(format, channels);
    double dx = (double) dst_rate / (double) src_rate;
    double x = dx;
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

    
