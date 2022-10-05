#ifndef __RESAMPLE_H__
#define __RESAMPLE_H__

#include <stdint.h>
#include <byteswap.h>
#include <alsa/asoundlib.h>

#include "resample.h"

extern void resample(snd_pcm_format_t format, size_t channels, 
		     size_t src_rate, void* src,
		     size_t dst_rate, void* dst,
		     size_t nframes, size_t dst_frames);

#endif
