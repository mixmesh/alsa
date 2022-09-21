#include <stdint.h>
#include <byteswap.h>
#include <alsa/asoundlib.h>

//#include "vector_types.h"

#define CAT_HELPER3(p,x,y) p ## x ## y
#define CAT3(p,x,y) CAT_HELPER3(p,x,y)

#define CAT_HELPER2(x,y) x ## y
#define CAT2(x,y) CAT_HELPER2(x,y)

#define MT_NAME(p,x) CAT3(p,NAME,x)
#define VTYPE        CAT2(v,TYPE)
#define VTYPE_R      CAT2(v,TYPE_R)
#define VTYPE_ZERO   CAT3(v,TYPE,_zero)
#define VTYPE_CONST(name) CAT3(v,TYPE,_const)(name)

#define TYPE_ZERO   CAT2(TYPE,_zero)

static inline float clamp_float01(float x)
{
    if (x >  1.0) return 1.0;
    if (x < -1.0) return -1.0;
    return x;
}

static inline float clamp_float_s16(float x)
{
    if (x >  32767.0) return 32767.0;
    if (x < -32767.0) return -32767.0;
    return x;
}

static inline int8_t clamp_int16_int8(int16_t x)
{
    if (x >  0x7f) return 0x7f;
    if (x < -0x7f) return -0x7f;
    return (int8_t) x;
}

static inline int16_t clamp_int32_int16(int32_t x)
{
    if (x >  0x7fff) return 0x7fff;
    if (x < -0x7fff) return -0x7fff;
    return (int16_t) x;
}

static inline int32_t clamp_int64_int32(int64_t x)
{
    if (x >  0x7fffffff) return 0x7fffffff;
    if (x < -0x7fffffff) return -0x7fffffff;
    return (int32_t) x;
}

static inline float clamp_double_float(double x)
{
    if (x > 1.0) return 1.0;
    if (x < -1.0) return -1.0;
    return (float) x;
}

static inline double clamp_double_double(double x)
{
    if (x > 1.0) return 1.0;
    if (x < -1.0) return -1.0;
    return (double) x;
}

#define PROCEDURE mix_pcm_int8
#define TYPE   int8_t
#define TYPE_R int16_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr) *(ptr)
#define CLAMP(sum) clamp_int16_int8(sum)
#include "mix_v.i"

#define PROCEDURE mix_pcm_uint8
#define TYPE   uint8_t
#define TYPE_R int16_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr) (*(ptr)-0x80)
#define CLAMP(sum) (uint8_t) (clamp_int16_int8(sum)+0x80)
#include "mix_v.i"


#define PROCEDURE mix_native_pcm_int16
#define TYPE   int16_t
#define TYPE_R int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr) *(ptr)
#define CLAMP(sum) clamp_int32_int16(sum)
#include "mix_v.i"

#define PROCEDURE mix_swap_pcm_int16
#define TYPE   int16_t
#define TYPE_R int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr) bswap_16(*(ptr))
#define CLAMP(sum) clamp_int32_int16(sum)
#include "mix_v.i"

#define PROCEDURE mix_native_pcm_uint16
#define TYPE   uint16_t
#define TYPE_R int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr) (*(ptr)-0x8000)
#define CLAMP(sum) (uint16_t)(clamp_int32_int16(sum)+0x8000)
#include "mix_v.i"

#define PROCEDURE mix_swap_pcm_uint16
#define TYPE   uint16_t
#define TYPE_R int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr) (bswap_16(*(ptr))-0x8000)
#define CLAMP(sum) (uint16_t)(clamp_int32_int16(sum)+0x8000)
#include "mix_v.i"

#define PROCEDURE mix_native_pcm_uint32
#define TYPE   uint32_t
#define TYPE_R int64_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr) (*(ptr)-0x80000000)
#define CLAMP(sum) (uint32_t)(clamp_int64_int32(sum)+0x80000000)
#include "mix_v.i"

#define PROCEDURE mix_swap_pcm_uint32
#define TYPE   uint32_t
#define TYPE_R int64_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr) (bswap_32(*(ptr))-0x80000000)
#define CLAMP(sum) (uint32_t)(clamp_int64_int32(sum)+0x80000000)
#include "mix_v.i"

#define PROCEDURE mix_native_pcm_int32
#define TYPE   int32_t
#define TYPE_R int64_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr) *(ptr)
#define CLAMP(sum) clamp_int64_int32(sum)
#include "mix_v.i"

#define PROCEDURE mix_swap_pcm_int32
#define TYPE   int32_t
#define TYPE_R int64_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr) bswap_32(*(ptr))
#define CLAMP(sum) clamp_int64_int32(sum)
#include "mix_v.i"

#define PROCEDURE mix_native_pcm_float
#define TYPE   float
#define TYPE_R double
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr) *(ptr)
#define CLAMP(sum) clamp_double_float(sum)
#include "mix_v.i"

#define PROCEDURE mix_swap_pcm_float
#define TYPE   float
#define TYPE_R double
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr) bswap_32(*(ptr))
#define CLAMP(sum) clamp_double_float(sum)
#include "mix_v.i"

#define PROCEDURE mix_native_pcm_double
#define TYPE   double
#define TYPE_R double
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr) *(ptr)
#define CLAMP(sum) clamp_double_double(sum)
#include "mix_v.i"

#define PROCEDURE mix_swap_pcm_double
#define TYPE   double
#define TYPE_R double
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr) bswap_64(*(ptr))
#define CLAMP(sum) clamp_double_double(sum)
#include "mix_v.i"

// mix samples from *src[i] and store in *dst
void mix(snd_pcm_format_t format, void** srcp, size_t channels, void* dst,
	 size_t n)
{
    switch(format) {
    case SND_PCM_FORMAT_S8:
	mix_pcm_int8((int8_t**)srcp, channels, (int8_t*) dst, n);
	break;
    case SND_PCM_FORMAT_U8:
	mix_pcm_uint8((uint8_t**)srcp, channels, (uint8_t*) dst, n);
	break;
    case SND_PCM_FORMAT_S16_LE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_native_pcm_int16((int16_t**)srcp, channels, (int16_t*) dst, n);
	break;
#else
	mix_swap_pcm_int16((int16_t**)srcp, channels, (int16_t*) dst, n);
	break;
#endif
    case SND_PCM_FORMAT_S16_BE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_swap_pcm_int16((int16_t**)srcp, channels, (int16_t*) dst, n);
	break;
#else
	mix_native_pcm_int16((int16_t**)srcp, channels, (int16_t*) dst, n);
	break;
#endif
    case SND_PCM_FORMAT_U16_LE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_native_pcm_uint16((uint16_t**)srcp, channels, (uint16_t*) dst, n);
	break;
#else
	mix_swap_pcm_uint16((uint16_t**)srcp, channels, (uint16_t*) dst, n);
	break;
#endif	
    case SND_PCM_FORMAT_U16_BE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_swap_pcm_uint16((uint16_t**)srcp, channels, (uint16_t*) dst, n);	
	break;
#else
	mix_native_pcm_uint16((uint16_t**)srcp, channels, (uint16_t*) dst, n);
	break;
#endif
    case SND_PCM_FORMAT_S32_LE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_native_pcm_int32((int32_t**)srcp, channels, (int32_t*) dst, n);
	break;
#else
	mix_swap_pcm_int32((int32_t**)srcp, channels, (int32_t*) dst, n);
	break;
#endif	
    case SND_PCM_FORMAT_S32_BE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_swap_pcm_int32((int32_t**)srcp, channels, (int32_t*) dst, n);
	break;
#else
v	mix_native_pcm_int32((int32_t**)srcp, channels, (int32_t*) dst, n);	
	break;
#endif	
    case SND_PCM_FORMAT_U32_LE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_native_pcm_uint32((uint32_t**)srcp, channels, (uint32_t*) dst, n);
	break;
#else
	mix_swap_pcm_uint32((uint32_t**)srcp, channels, (uint32_t*) dst, n);
	break;
#endif		
    case SND_PCM_FORMAT_U32_BE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_swap_pcm_uint32((uint32_t**)srcp, channels, (uint32_t*) dst, n);	
	break;
#else
	mix_native_pcm_uint32((uint32_t**)srcp, channels, (uint32_t*) dst, n);
	break;
#endif			
    case SND_PCM_FORMAT_FLOAT_LE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_native_pcm_float((float**)srcp, channels, (float*) dst, n);
	break;
#else
	mix_swap_pcm_float((float**)srcp, channels, (float*) dst, n);
	break;
#endif
    case SND_PCM_FORMAT_FLOAT_BE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_swap_pcm_float((float**)srcp, channels, (float*) dst, n);
	break;
#else
	mix_native_pcm_float((float**)srcp, channels, (float*) dst, n);
	break;
#endif			
    case SND_PCM_FORMAT_FLOAT64_LE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_native_pcm_double((double**)srcp, channels, (double*) dst, n);
	break;
#else
	mix_swap_pcm_double((double**)srcp, channels, (double*) dst, n);
	break;
#endif	
    case SND_PCM_FORMAT_FLOAT64_BE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_swap_pcm_double((double**)srcp, channels, (double*) dst, n);	
	break;
#else
	mix_native_pcm_double((double**)srcp, channels, (double*) dst, n);
	break;
#endif		
    default:
	break;
    }
}

#ifdef TEST

#define NUM_VOICES   4
#define NUM_FRAMES   1024
#define NUM_CHANNELS 5
#define BUF_SIZE      (sizeof(int16_t)*NUM_FRAMES*NUM_CHANNELS)
#define NUM_REPEATS  10000

#if __BYTE_ORDER == __LITTLE_ENDIAN
#define SND_PCM_FORMAT_S16_NATIVE SND_PCM_FORMAT_S16_LE
#else
#define SND_PCM_FORMAT_S16_NATIVE SND_PCM_FORMAT_S16_BE
#endif

#if __BYTE_ORDER == __LITTLE_ENDIAN
#define SND_PCM_FORMAT_S16_SWAP SND_PCM_FORMAT_S16_BE
#else
#define SND_PCM_FORMAT_S16_SWAP SND_PCM_FORMAT_S16_LE
#endif

#include <sys/time.h>
uint64_t time_tick(void)
{
    struct timeval t;
    gettimeofday(&t, 0);
    return t.tv_sec*(uint64_t)1000000 + t.tv_usec;
}

void test_mix_native(int16_t** smv, int16_t* dst, size_t n)
{
    int16_t* smv_tmp[NUM_VOICES];
    while(n--) {
	memcpy(smv_tmp, smv, sizeof(int16_t*)*NUM_VOICES);
	mix(SND_PCM_FORMAT_S16_NATIVE,
	    (void**) smv_tmp, NUM_VOICES,
	    (void*) dst, NUM_FRAMES*NUM_CHANNELS);
    }
}

void test_mix_swap(int16_t** smv, int16_t* dst, size_t n)
{
    int16_t* smv_tmp[NUM_VOICES];
    while(n--) {
	memcpy(smv_tmp, smv, sizeof(int16_t*)*NUM_VOICES);
	mix(SND_PCM_FORMAT_S16_SWAP,
	    (void**) smv_tmp, NUM_VOICES,
	    (void*) dst, NUM_FRAMES*NUM_CHANNELS);
    }
}

void validate_mix(int16_t** smv, int16_t* dst)
{
    int16_t* smv_tmp[NUM_VOICES];
    int i;
    
    memcpy(smv_tmp, smv, sizeof(int16_t*)*NUM_VOICES);
    mix(SND_PCM_FORMAT_S16_NATIVE,
	(void**) smv_tmp, NUM_VOICES,
	(void*) dst, NUM_FRAMES*NUM_CHANNELS);
    for (i = 0; i < NUM_FRAMES*NUM_CHANNELS; i++) {
	if (dst[i] != (0x0001 + 0x0002 + 0x0003 + 0x0004)) {
	    printf("error: samples %d not mixed %d\n", i, dst[i]);
	    exit(1);
	}
    }

    memcpy(smv_tmp, smv, sizeof(int16_t*)*NUM_VOICES);    
    mix(SND_PCM_FORMAT_S16_SWAP,
	(void**) smv_tmp, NUM_VOICES,
	(void*) dst, NUM_FRAMES*NUM_CHANNELS);
    for (i = 0; i < NUM_FRAMES*NUM_CHANNELS; i++) {
	if (dst[i] != (0x0100 + 0x0200 + 0x0300 + 0x0400)) {
	    printf("error: swapped samples %d not mixed %d\n", i, dst[i]);
	    exit(1);
	}
    }    
}

int main(int argc, char** argv)
{
    int16_t* smv[NUM_VOICES];
    int16_t  dst[BUF_SIZE];
    int i;
    uint64_t t0, t1;

    for (i = 0; i < NUM_VOICES; i++) {
	int j;
	int16_t* ptr;
	ptr = (int16_t*) malloc(BUF_SIZE);
	smv[i] = ptr;
	for (j = 0; j < NUM_FRAMES*NUM_CHANNELS; j++) {
	    ptr[j] = (i+1);
	}
    }

    validate_mix(smv, dst);
    
    t0 = time_tick();
    test_mix_native(smv, dst, NUM_REPEATS);
    t1 = time_tick();
    printf("time = %lus+%luus, %f mixes/s\n",
	   ((unsigned long)(t1-t0)) / 1000000,
	   ((unsigned long)(t1-t0)) % 1000000,
	   ((float)NUM_REPEATS / (((unsigned long)(t1-t0))/1000000.0)));

    t0 = time_tick();
    test_mix_swap(smv, dst, NUM_REPEATS);
    t1 = time_tick();
    printf("time = %lus+%luus, %f swapped mixes/s\n",
	   ((unsigned long)(t1-t0)) / 1000000,
	   ((unsigned long)(t1-t0)) % 1000000,
	   ((float)NUM_REPEATS / (((unsigned long)(t1-t0))/1000000.0)));
    exit(0);
}

#endif
