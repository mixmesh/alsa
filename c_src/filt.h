#ifndef __FILT_H__
#define __FILT_H__

#include <stdint.h>
#include <alsa/asoundlib.h>

extern void filter(void* src, snd_pcm_format_t src_format, size_t src_len,
		   double* filter, size_t filter_len,
		   void* dst, snd_pcm_format_t dst_format, size_t dst_len);

#endif
