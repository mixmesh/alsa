#ifndef __REFORMAT_H__
#define __REFORMAT_H__

#include <stdint.h>
#include <alsa/asoundlib.h>

extern void write_pcm_int(snd_pcm_format_t format, int32_t val, int8_t* ptr);
extern void write_pcm_float(snd_pcm_format_t format,double d, int8_t* ptr);
extern int32_t read_pcm_int(snd_pcm_format_t format, int8_t* ptr);
extern double read_pcm_float(snd_pcm_format_t format, int8_t* ptr);

extern void reformat(snd_pcm_format_t src_format, size_t src_channels, void* src,
		     snd_pcm_format_t dst_format, size_t dst_channels, void* dst,
		     size_t num_frames);

#endif
