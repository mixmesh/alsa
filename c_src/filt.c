// Filter implementation

#include "filt.h"
#include "reformat.h"

// note that the filter coeffients are reversed
void filter(void* src, snd_pcm_format_t src_format, size_t src_len,
	    double* filter, size_t filter_len,
	    void* dst, snd_pcm_format_t dst_format, size_t dst_len)
{
    int n, k;
    int8_t* sp = (int8_t*) src; 
    int8_t* dp = (int8_t*) dst;   
    ssize_t src_size = snd_pcm_format_size(src_format, 1);
    ssize_t dst_size = snd_pcm_format_size(dst_format, 1);
    size_t len = (src_len < dst_len) ? src_len : dst_len;

    n = 0;
    while((n < filter_len) && (n < len)) {
	double sum = 0;
	int8_t* xp = sp;
	sp += src_size;
	for (k = 0; k <= n; k++) {
	    double x = read_pcm_float(src_format, xp);
	    double ck = filter[k];
	    sum += x*ck;
	    xp -= src_size;
	}
	write_pcm_float(dst_format, sum, dp);
	dp += dst_size;
	n++;
    }
    while(n < len) {
	double sum = 0;
	int8_t* xp = sp;
	sp += src_size;
	for (k = 0; k <= filter_len; k++) {
	    double x = read_pcm_float(src_format, xp);
	    double ck = filter[k];
	    sum += x*ck;
	    xp -= src_size;
	}
	write_pcm_float(dst_format, sum, dp);
	dp += dst_size;
	n++;
    }
    // fill with zeros
    while(n < dst_len) {
	double sum = 0;
	write_pcm_float(dst_format, sum, dp);
	dp += dst_size;
	n++;
    }
}
