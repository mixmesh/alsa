#ifndef __MIX_H__
#define __MIX_H__

#include <stdint.h>
#include <alsa/asoundlib.h>

extern void mix(snd_pcm_format_t format, void** srcp, size_t num_voices,
		void* dst, size_t num_samples);

#endif
