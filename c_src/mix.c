#include <stdint.h>
#include <byteswap.h>
#include <alsa/asoundlib.h>

#include "aulaw.h"

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

static inline int8_t clamp_int16_int8(int16_t x)
{
    if (x >  0x7f) return  0x7f;
    if (x < -0x7f) return -0x7f;
    return (int8_t) x;
}

static inline int32_t clamp_int32_int24(int32_t x)
{
    if (x >  0x7fffff) return  0x7fffff;
    if (x < -0x7fffff) return -0x7fffff;
    return (int32_t) x;
}


static inline int16_t clamp_int32_int16(int32_t x)
{
    if (x >  0x7fff) return  0x7fff;
    if (x < -0x7fff) return -0x7fff;
    return (int16_t) x;
}

static inline int32_t clamp_int64_int32(int64_t x)
{
    if (x >  0x7fffffff) return  0x7fffffff;
    if (x < -0x7fffffff) return -0x7fffffff;
    return (int32_t) x;
}

static inline float clamp_double_float(double x)
{
    if (x > 1.0)  return  1.0;
    if (x < -1.0) return -1.0;
    return (float) x;
}

static inline double clamp_double_double(double x)
{
    if (x > 1.0)  return  1.0;
    if (x < -1.0) return -1.0;
    return (double) x;
}

// read two-complement integer samples 8,16,24,32
static inline int8_t rd_int8(uint8_t* ptr)
{
    return *((int8_t*)ptr);
}

static inline int16_t rd_int16(uint8_t* ptr)
{
    return *((int16_t*)ptr);
}

static inline int16_t rd_int16_swap(uint8_t* ptr)
{
    return (int16_t) bswap_16(*((uint16_t*)ptr));
}

static inline int32_t rd_int24(uint8_t* ptr)
{
    return (*((int32_t*)ptr) << 8) >> 8;
}

static inline int32_t rd_int24_swap(uint8_t* ptr)
{
    return (((int32_t) bswap_32(*((uint32_t*)ptr)))<<8)>>8;
}

static inline int32_t rd_int32(uint8_t* ptr)
{
    return *((int32_t*)ptr);
}

static inline int32_t rd_int32_swap(uint8_t* ptr)
{
    return (int32_t) bswap_32(*((uint32_t*)ptr));
}

static inline float rd_float(uint8_t* ptr)
{
    return *((float*)ptr);
}

static inline float rd_float_swap(uint8_t* ptr)
{
    union {
	uint32_t x;
	float f;
    } u;
    u.x = bswap_32(*(uint32_t*)ptr);
    return u.f;
}

static inline double rd_double(uint8_t* ptr)
{
    return *((double*)ptr);
}

static inline double rd_double_swap(uint8_t* ptr)
{
    union {
	uint64_t x;
	double f;
    } u;
    u.x = bswap_64(*(uint64_t*)ptr);
    return u.f;
}

// read one-complement integer samples 8,16,24,32
static inline int8_t rd_uint8(uint8_t* ptr)
{
    return *((uint8_t*)ptr)-0x80;
}

static inline int16_t rd_uint16(uint8_t* ptr)
{
    return *((uint16_t*)ptr)-0x8000;
}

static inline int16_t rd_uint16_swap(uint8_t* ptr)
{
    return (bswap_16(*((uint16_t*)ptr))-0x8000);
}

static inline int32_t rd_uint24(uint8_t* ptr)
{
    return *((uint32_t*)ptr) - 0x800000; // mask?
}

static inline int32_t rd_uint24_swap(uint8_t* ptr)
{
    return bswap_32(*((uint32_t*)ptr)) - 0x800000; // mask?
}

static inline int32_t rd_uint32(uint8_t* ptr)
{
    return *((uint32_t*)ptr)-0x80000000;
}

static inline int32_t rd_uint32_swap(uint8_t* ptr)
{
    return bswap_32(*((uint32_t*)ptr))-0x80000000;
}

static inline void wr_int8(uint8_t* ptr, int16_t a)
{
    *((int8_t*)ptr) = clamp_int16_int8(a);
}

static inline void wr_int16(uint8_t* ptr, int32_t a)
{
    *((int16_t*)ptr) = clamp_int32_int16(a);
}

static inline void wr_int16_swap(uint8_t* ptr, int32_t a)
{
    *((int16_t*)ptr) = bswap_16(clamp_int32_int16(a));
}

static inline void wr_int24(uint8_t* ptr, int32_t a)
{
    *((int32_t*)ptr) = clamp_int32_int24(a);
}

static inline void wr_int24_swap(uint8_t* ptr, int32_t a)
{
    *((int32_t*)ptr) = bswap_32(clamp_int32_int24(a));
}

static inline void wr_int32(uint8_t* ptr, int64_t a)
{
    *((int32_t*)ptr) = clamp_int64_int32(a);
}

static inline void wr_int32_swap(uint8_t* ptr, int64_t a)
{
    *((int32_t*)ptr) = bswap_32(clamp_int64_int32(a));
}

static inline void wr_float(uint8_t* ptr, double a)
{
    *((float*)ptr) = clamp_double_float(a);
}

static inline void wr_float_swap(uint8_t* ptr, double a)
{
    union {
	uint32_t x;
	float f;
    } u;
    u.f = clamp_double_float(a);
    *((uint32_t*)ptr) = bswap_32(u.x);
}

static inline void wr_double(uint8_t* ptr, double a)
{    
    *((double*)ptr) = clamp_double_double(a);
}

static inline void wr_double_swap(uint8_t* ptr, double a)
{
    union {
	uint64_t x;
	double f;
    } u;
    u.f = clamp_double_double(a);
    *((uint32_t*)ptr) = bswap_64(u.x);
}

static inline void wr_uint8(uint8_t* ptr, int16_t a)
{
    *((uint8_t*)ptr) = clamp_int16_int8(a)+0x80;
}

static inline void wr_uint16(uint8_t* ptr, int32_t a)
{
    *((uint16_t*)ptr) = clamp_int32_int16(a) + 0x8000;
}

static inline void wr_uint16_swap(uint8_t* ptr, int32_t a)
{
    *((uint16_t*)ptr) = bswap_16(clamp_int32_int16(a) + 0x8000);
}

static inline void wr_uint24(uint8_t* ptr, int32_t a)
{
    *((uint32_t*)ptr) = clamp_int32_int24(a) + 0x800000;
}

static inline void wr_uint24_swap(uint8_t* ptr, int32_t a)
{
    *((uint32_t*)ptr) = bswap_32(clamp_int32_int24(a) + 0x800000);
}

static inline void wr_uint32(uint8_t* ptr, int64_t a)
{
    *((uint32_t*)ptr) = clamp_int64_int32(a) + 0x80000000;
}

static inline void wr_uint32_swap(uint8_t* ptr, int64_t a)
{
    *((uint32_t*)ptr) = bswap_32(clamp_int64_int32(a) + 0x80000000);
}

static inline uint32_t rd_3le(uint8_t* ptr)
{
    return (ptr[0] | (ptr[1]<<8) | (ptr[2]<<16));
}

static inline uint32_t rd_3be(uint8_t* ptr)
{
    return (ptr[2] | (ptr[1]<<8) | (ptr[0]<<16));
}

static inline void wr_3le(uint8_t* ptr, uint32_t val)
{
    ptr[0] = val;
    ptr[1] = val >> 8;
    ptr[2] = val >> 16;
}

static inline void wr_3be(uint8_t* ptr, uint32_t val)
{
    ptr[2] = val;
    ptr[1] = val >> 8;
    ptr[0] = val >> 16;
}

static inline int16_t add_int8(int16_t a, int16_t b)
{
    return a+b;
}

static inline int32_t add_int16(int32_t a, int32_t b)
{
    return a+b;
}

static inline int32_t add_int24(int32_t a, int32_t b)
{
    return a+b;
}

static inline int32_t add_int32(int32_t a, int32_t b)
{
    return a+b;
}

static inline float add_float(float a, float b)
{
    return a+b;
}

static inline double add_double(double a, double b)
{
    return a+b;
}


#define PROCEDURE mix_pcm_int8
#define SAMPLE_SIZE 1
#define TYPE  int8_t
#define ITYPE int16_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_int8((ptr))
#define WRITE(ptr,a) wr_int8((ptr),(a))
#define MIX2(a,b)    add_int8((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_pcm_uint8
#define SAMPLE_SIZE 1
#define TYPE  uint8_t
#define ITYPE int16_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_uint8((ptr))
#define WRITE(ptr,a) wr_uint8((ptr),(a))
#define MIX2(a,b)    add_int8((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_a_law
#define SAMPLE_SIZE 1
#define TYPE  int16_t
#define ITYPE int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    a_law_table[*(ptr)]
#define WRITE(ptr,a) *(ptr) = a_law_encode(clamp_int32_int16((a)))
#define MIX2(a,b)    add_int16((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_mu_law
#define SAMPLE_SIZE 1
#define TYPE  int16_t
#define ITYPE int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    mu_law_table[*(ptr)]
#define WRITE(ptr,a) *(ptr) = mu_law_encode(clamp_int32_int16((a)))
#define MIX2(a,b)    add_int16((a),(b))
#include "mix_v.i"


#define PROCEDURE mix_native_pcm_int16
#define SAMPLE_SIZE 2
#define TYPE  int16_t
#define ITYPE int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_int16((ptr))
#define WRITE(ptr,a) wr_int16((ptr),(a))
#define MIX2(a,b)    add_int16((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_swap_pcm_int16
#define SAMPLE_SIZE 2
#define TYPE  int16_t
#define ITYPE int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_int16_swap((ptr))
#define WRITE(ptr,a) wr_int16_swap((ptr),(a))
#define MIX2(a,b)    add_int16((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_native_pcm_uint16
#define SAMPLE_SIZE 2
#define TYPE  uint16_t
#define ITYPE int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_uint16((ptr))
#define WRITE(ptr,a) wr_uint16((ptr),(a))
#define MIX2(a,b)    add_int16((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_swap_pcm_uint16
#define SAMPLE_SIZE 2
#define TYPE  uint16_t
#define ITYPE int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)     rd_uint16_swap((ptr))
#define WRITE(ptr,a)  wr_uint16_swap((ptr),(a))
#define MIX2(a,b)     add_int16((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_pcm_int24_3le
#define SAMPLE_SIZE 3
#define TYPE  int32_t
#define ITYPE int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)   (((int32_t)rd_3le((ptr))<<8)>>8)
#define WRITE(ptr,a) wr_3le((ptr),(uint32_t)((a)))
#define MIX2(a,b)    add_int32((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_pcm_int24_3be
#define SAMPLE_SIZE 3
#define TYPE  int32_t
#define ITYPE int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)   (((int32_t)rd_3be((ptr))<<8)>>8)
#define WRITE(ptr,a) wr_3be((ptr),(uint32_t)((a)))
#define MIX2(a,b)    add_int32((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_pcm_uint24_3le
#define SAMPLE_SIZE 3
#define TYPE  int32_t
#define ITYPE int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)   (((int32_t)(rd_3le((ptr))-0x80000)<<8)>>8)
#define WRITE(ptr,a) wr_3le((ptr),(uint32_t)((a)+0x800000))
#define MIX2(a,b)    add_int32((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_pcm_uint24_3be
#define SAMPLE_SIZE 3
#define TYPE  int32_t
#define ITYPE int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)   (((int32_t)(rd_3be((ptr))-0x800000)<<8)>>8)
#define WRITE(ptr,a) wr_3be((ptr),(uint32_t)((a)+0x800000))
#define MIX2(a,b)    add_int32((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_native_pcm_int24
#define SAMPLE_SIZE 4
#define TYPE  int32_t
#define ITYPE int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_int24((ptr))
#define WRITE(ptr,a) wr_int24((ptr),(a))
#define MIX2(a,b)    add_int24((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_swap_pcm_int24
#define SAMPLE_SIZE 4
#define TYPE  int32_t
#define ITYPE int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_int24_swap((ptr))
#define WRITE(ptr,a) wr_int24_swap((ptr),(a))
#define MIX2(a,b)    add_int24((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_native_pcm_uint24
#define SAMPLE_SIZE 4
#define TYPE  uint32_t
#define ITYPE int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_uint24((ptr))
#define WRITE(ptr,a) wr_uint24((ptr),(a))
#define MIX2(a,b)    add_int24((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_swap_pcm_uint24
#define SAMPLE_SIZE 4
#define TYPE  uint32_t
#define ITYPE int32_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_uint24_swap((ptr))
#define WRITE(ptr,a) wr_uint24_swap((ptr),(a))
#define MIX2(a,b)    add_int24((a),(b))
#include "mix_v.i"



#define PROCEDURE mix_native_pcm_int32
#define SAMPLE_SIZE 4
#define TYPE  int32_t
#define ITYPE int64_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_int32((ptr))
#define WRITE(ptr,a) wr_int32((ptr),(a))
#define MIX2(a,b)    add_int32((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_swap_pcm_int32
#define SAMPLE_SIZE 4
#define TYPE  int32_t
#define ITYPE int64_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_int32_swap((ptr))
#define WRITE(ptr,a) wr_int32_swap((ptr),(a))
#define MIX2(a,b)    add_int32((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_native_pcm_uint32
#define SAMPLE_SIZE 4
#define TYPE  uint32_t
#define ITYPE int64_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_uint32((ptr))
#define WRITE(ptr,a) wr_uint32((ptr),(a))
#define MIX2(a,b)    add_int32((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_swap_pcm_uint32
#define SAMPLE_SIZE 4
#define TYPE  uint32_t
#define ITYPE int64_t
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_uint32_swap((ptr))
#define WRITE(ptr,a) wr_uint32_swap((ptr),(a))
#define MIX2(a,b)    add_int32((a),(b))
#include "mix_v.i"


#define PROCEDURE mix_native_pcm_float
#define SAMPLE_SIZE 4
#define TYPE  float
#define ITYPE float
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_float((ptr))
#define WRITE(ptr,a) wr_float((ptr),(a))
#define MIX2(a,b)    add_float((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_swap_pcm_float
#define SAMPLE_SIZE 4
#define TYPE  float
#define ITYPE float
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_float_swap((ptr))
#define WRITE(ptr,a) wr_float_swap((ptr),(a))
#define MIX2(a,b)    add_float((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_native_pcm_double
#define SAMPLE_SIZE 8
#define TYPE  double
#define ITYPE double
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_double((ptr))
#define WRITE(ptr,a) wr_double((ptr),(a))
#define MIX2(a,b)    add_double((a),(b))
#include "mix_v.i"

#define PROCEDURE mix_swap_pcm_double
#define SAMPLE_SIZE 8
#define TYPE  double
#define ITYPE double
#define PARAMS_DECL
#define LOCALS_DECL
#define READ(ptr)    rd_double_swap((ptr))
#define WRITE(ptr,a) wr_double_swap((ptr),(a))
#define MIX2(a,b)    add_double((a),(b))
#include "mix_v.i"

// mix samples from *src[i] and store in *dst
void mix(snd_pcm_format_t format, void** srcp, double* vol, size_t num_voices,
	 void* dst, size_t n)
{
    switch(format) {
	// 8-bit
    case SND_PCM_FORMAT_S8:
	mix_pcm_int8((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
    case SND_PCM_FORMAT_U8:
	mix_pcm_uint8((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
    case SND_PCM_FORMAT_A_LAW:
	mix_a_law((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
    case SND_PCM_FORMAT_MU_LAW:
	mix_mu_law((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
	// 16-bit
    case SND_PCM_FORMAT_S16_LE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_native_pcm_int16((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#else
	mix_swap_pcm_int16((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#endif
    case SND_PCM_FORMAT_S16_BE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_swap_pcm_int16((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#else
	mix_native_pcm_int16((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#endif
    case SND_PCM_FORMAT_U16_LE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_native_pcm_uint16((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#else
	mix_swap_pcm_uint16((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#endif	
    case SND_PCM_FORMAT_U16_BE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_swap_pcm_uint16((uint8_t**)srcp, vol, num_voices, dst, n);	
	break;
#else
	mix_native_pcm_uint16((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#endif
	// 24-bit (int 32 bit)
    case SND_PCM_FORMAT_S24_LE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_native_pcm_int24((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#else
	mix_swap_pcm_int24((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#endif
    case SND_PCM_FORMAT_S24_BE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_swap_pcm_int24((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#else
	mix_native_pcm_int24((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#endif
    case SND_PCM_FORMAT_U24_LE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_native_pcm_uint24((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#else
	mix_swap_pcm_uint24((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#endif	
    case SND_PCM_FORMAT_U24_BE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_swap_pcm_uint24((uint8_t**)srcp, vol, num_voices, dst, n);	
	break;
#else
	mix_native_pcm_uint24((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#endif
    case SND_PCM_FORMAT_S24_3LE:
	mix_pcm_int24_3le((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
    case SND_PCM_FORMAT_S24_3BE:
	mix_pcm_int24_3be((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
    case SND_PCM_FORMAT_U24_3LE:
	mix_pcm_uint24_3le((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
    case SND_PCM_FORMAT_U24_3BE:
	mix_pcm_uint24_3be((uint8_t**)srcp, vol, num_voices, dst, n);
	break;		
	// 32-bit
    case SND_PCM_FORMAT_S32_LE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_native_pcm_int32((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#else
	mix_swap_pcm_int32((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#endif	
    case SND_PCM_FORMAT_S32_BE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_swap_pcm_int32((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#else
v	mix_native_pcm_int32((uint8_t**)srcp, vol, num_voices, dst, n);	
	break;
#endif	
    case SND_PCM_FORMAT_U32_LE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_native_pcm_uint32((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#else
	mix_swap_pcm_uint32((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#endif		
    case SND_PCM_FORMAT_U32_BE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_swap_pcm_uint32((uint8_t**)srcp, vol, num_voices, dst, n);	
	break;
#else
	mix_native_pcm_uint32((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#endif
    case SND_PCM_FORMAT_FLOAT_LE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_native_pcm_float((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#else
	mix_swap_pcm_float((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#endif
    case SND_PCM_FORMAT_FLOAT_BE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_swap_pcm_float((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#else
	mix_native_pcm_float((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#endif
    case SND_PCM_FORMAT_FLOAT64_LE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_native_pcm_double((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#else
	mix_swap_pcm_double((uint8_t**)srcp, vol, num_voices, dst, n);
	break;
#endif
    case SND_PCM_FORMAT_FLOAT64_BE:
#if __BYTE_ORDER == __LITTLE_ENDIAN
	mix_swap_pcm_double((uint8_t**)srcp, vol, num_voices, dst, n);	
	break;
#else
	mix_native_pcm_double((uint8_t**)srcp, vol, num_voices, dst, n);
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

void test_mix_native(int16_t** smv, double* vol, int16_t* dst, size_t n)
{
    while(n--) {
	mix(SND_PCM_FORMAT_S16_NATIVE,
	    (void**) smv, vol, NUM_VOICES,
	    (void*) dst, NUM_FRAMES*NUM_CHANNELS);
    }
}

void test_mix_swap(int16_t** smv, double* vol, int16_t* dst, size_t n)
{
    while(n--) {
	mix(SND_PCM_FORMAT_S16_SWAP,
	    (void**) smv, vol, NUM_VOICES,
	    (void*) dst, NUM_FRAMES*NUM_CHANNELS);
    }
}

#define VVOICE 4
#define VSAMP 128

void validate_s8()
{
    int8_t  src[VSAMP];
    int8_t  dst[VSAMP];
    int8_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = 1;

    mix(SND_PCM_FORMAT_S8, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    
    for (j = 0; j < VSAMP; j++) {    
	if (dst[j] != 4) {
	    printf("error: S8 samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}

void validate_u8()
{
    uint8_t  src[VSAMP];
    uint8_t  dst[VSAMP];
    uint8_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };    
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = 0x81;
    mix(SND_PCM_FORMAT_U8, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {
	if (dst[j] != 0x84) {
	    printf("error: U8 samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}

void validate_a_law()
{
    uint8_t  src[VSAMP];
    uint8_t  dst[VSAMP];
    uint8_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = a_law_encode(8);
    mix(SND_PCM_FORMAT_A_LAW, (void**) smv, vol,VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {
	if (dst[j] != 0xd7) {  // encode(8+8+8+8)
	    printf("error: A_LAW samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}

void validate_mu_law()
{
    uint8_t  src[VSAMP];
    uint8_t  dst[VSAMP];
    uint8_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = mu_law_encode(8);
    mix(SND_PCM_FORMAT_MU_LAW, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {
	if (dst[j] != 0xfb) {  // encode(8+8+8+8)
	    printf("error: MU_LAW samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}


void validate_s16_le()
{
    int16_t  src[VSAMP];
    int16_t  dst[VSAMP];
    int16_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = 0x0001;
    mix(SND_PCM_FORMAT_S16_LE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {    
	if (dst[j] != 0x0004) {
	    printf("error: S16_LE samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}

void validate_s16_be()
{
    int16_t  src[VSAMP];
    int16_t  dst[VSAMP];
    int16_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = 0x0100;
    mix(SND_PCM_FORMAT_S16_BE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {    
	if (dst[j] != 0x0400) {
	    printf("error: S16_BE samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}

void validate_u16_le()
{
    uint16_t  src[VSAMP];
    uint16_t  dst[VSAMP];
    uint16_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = 0x8001;
    mix(SND_PCM_FORMAT_U16_LE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {    
	if (dst[j] != 0x8004) {
	    printf("error: U16_LE samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}

void validate_u16_be()
{
    uint16_t  src[VSAMP];
    uint16_t  dst[VSAMP];
    uint16_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = 0x0081;
    mix(SND_PCM_FORMAT_U16_BE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {    
	if (dst[j] != 0x0084) {
	    printf("error: U16_BE samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}

void validate_s24_le()
{
    int32_t  src[VSAMP];
    int32_t  dst[VSAMP];
    int32_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = 0x000001;
    mix(SND_PCM_FORMAT_S24_LE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {    
	if (dst[j] != 0x000004) {
	    printf("error: S24_LE samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}

void validate_s24_be()
{
    int32_t  src[VSAMP];
    int32_t  dst[VSAMP];
    int32_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = 0x000100;
    mix(SND_PCM_FORMAT_S24_BE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {    
	if (dst[j] != 0x000400) {
	    printf("error: S24_BE samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}


void validate_u24_le()
{
    uint32_t  src[VSAMP];
    uint32_t  dst[VSAMP];
    uint32_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = 0x800001;
    mix(SND_PCM_FORMAT_U24_LE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {    
	if (dst[j] != 0x800004) {
	    printf("error: U24_LE samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}

void validate_u24_be()
{
    int32_t  src[VSAMP];
    int32_t  dst[VSAMP];
    int32_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = 0x00008100;
    mix(SND_PCM_FORMAT_U24_BE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {    
	if (dst[j] != 0x00008400) {
	    printf("error: U24_BE samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}

void validate_s24_3le()
{
    uint8_t  src[3*VSAMP];
    uint8_t  dst[3*VSAMP];
    uint8_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j += 3) {
	src[j]   = 0x01; src[j+1] = 0x00; src[j+2] = 0x00;
    }
    mix(SND_PCM_FORMAT_S24_3LE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j += 3) {
	if ((dst[j] != 0x04) && (dst[j+1] != 0x00) && (dst[j+2] != 0x00)) {
	    printf("error: S24_3LE samples %d not mixed %02x%02x%02x\n",
		   j/3, dst[j],dst[j+1],dst[j+2]);
	    break;
	}
    }
}

void validate_s24_3be()
{
    uint8_t  src[3*VSAMP];
    uint8_t  dst[3*VSAMP];
    uint8_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j += 3) {
	src[j]   = 0x00; src[j+1] = 0x00; src[j+2] = 0x01;
    }
    mix(SND_PCM_FORMAT_S24_3BE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j += 3) {
	if ((dst[j] != 0x00) && (dst[j+1] != 0x00) && (dst[j+2] != 0x01)) {
	    printf("error: S24_3LE samples %d not mixed %02x%02x%02x\n",
		   j/3, dst[j],dst[j+1],dst[j+2]);
	    break;
	}
    }
}

void validate_u24_3le()
{
    uint8_t  src[3*VSAMP];
    uint8_t  dst[3*VSAMP];
    uint8_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j += 3) {
	src[j]   = 0x01; src[j+1] = 0x00; src[j+2] = 0x80;
    }
    mix(SND_PCM_FORMAT_U24_3LE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j += 3) {
	if ((dst[j] != 0x04) && (dst[j+1] != 0x00) && (dst[j+2] != 0x80)) {
	    printf("error: U24_3LE samples %d not mixed %02x%02x%02x\n",
		   j/3, dst[j],dst[j+1],dst[j+2]);
	    break;
	}
    }
}

void validate_u24_3be()
{
    uint8_t  src[3*VSAMP];
    uint8_t  dst[3*VSAMP];
    uint8_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };    
    int j;

    for (j = 0; j < VSAMP; j += 3) {
	src[j]   = 0x80; src[j+1] = 0x00; src[j+2] = 0x01;
    }
    mix(SND_PCM_FORMAT_U24_3BE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j += 3) {
	if ((dst[j] != 0x80) && (dst[j+1] != 0x00) && (dst[j+2] != 0x01)) {
	    printf("error: U24_3LE samples %d not mixed %02x%02x%02x\n",
		   j/3, dst[j],dst[j+1],dst[j+2]);
	    break;
	}
    }
}


void validate_s32_le()
{
    int32_t  src[VSAMP];
    int32_t  dst[VSAMP];
    int32_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = 0x00000001;
    mix(SND_PCM_FORMAT_S32_LE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {    
	if (dst[j] != 0x00000004) {
	    printf("error: S32_LE samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}

void validate_s32_be()
{
    int32_t  src[VSAMP];
    int32_t  dst[VSAMP];
    int32_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = 0x00000100;
    mix(SND_PCM_FORMAT_S32_BE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {    
	if (dst[j] != 0x00000400) {
	    printf("error: S32_BE samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}

void validate_u32_le()
{
    uint32_t  src[VSAMP];
    uint32_t  dst[VSAMP];
    uint32_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = 0x80000001;
    mix(SND_PCM_FORMAT_U32_LE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {    
	if (dst[j] != 0x80000004) {
	    printf("error: U32_LE samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}

void validate_u32_be()
{
    uint32_t  src[VSAMP];
    uint32_t  dst[VSAMP];
    uint32_t* smv[VVOICE] = { src, src, src, src };
    double  vol[VVOICE] = { 1.0, 1.0, 1.0, 1.0 };
    int j;

    for (j = 0; j < VSAMP; j++)
	src[j] = 0x00000081;
    mix(SND_PCM_FORMAT_U32_BE, (void**) smv, vol, VVOICE, (void*) dst, VSAMP);
    for (j = 0; j < VSAMP; j++) {    
	if (dst[j] != 0x00000084) {
	    printf("error: U32_BE samples %d not mixed %x\n",
		   j, dst[j]);
	    break;
	}
    }
}

void validate_8()
{
    validate_s8();
    validate_u8();
    validate_mu_law();
    validate_a_law();
}

void validate_16()
{
    validate_s16_le();
    validate_s16_be();
    validate_u16_le();
    validate_u16_be();    
}

void validate_24()
{
    validate_s24_le();
    validate_s24_be();
    validate_u24_le();
    validate_u24_be();
    // 3bytes
    validate_s24_3le();
    validate_s24_3be();
    validate_u24_3le();
    validate_u24_3be();    
}

void validate_32()
{
    validate_s32_le();
    validate_s32_be();
    validate_u32_le();
    validate_u32_be();    
}


int main(int argc, char** argv)
{
    int16_t* smv[NUM_VOICES];
    double   vol[NUM_VOICES];
    int16_t  dst[BUF_SIZE];
    int i;
    uint64_t t0, t1;

    for (i = 0; i < NUM_VOICES; i++) {
	int j;
	int16_t* ptr;
	ptr = (int16_t*) malloc(BUF_SIZE);
	smv[i] = ptr;
	vol[i] = 1.0;
	for (j = 0; j < NUM_FRAMES*NUM_CHANNELS; j++) {
	    ptr[j] = (i+1);
	}
    }    

    validate_8();
    validate_16();
    validate_24();
    validate_32();    

    t0 = time_tick();
    test_mix_native(smv, vol, dst, NUM_REPEATS);
    t1 = time_tick();
    printf("time = %lus+%luus, %f mixes/s\n",
	   ((unsigned long)(t1-t0)) / 1000000,
	   ((unsigned long)(t1-t0)) % 1000000,
	   ((float)NUM_REPEATS / (((unsigned long)(t1-t0))/1000000.0)));

    t0 = time_tick();
    test_mix_swap(smv, vol, dst, NUM_REPEATS);
    t1 = time_tick();
    printf("time = %lus+%luus, %f swapped mixes/s\n",
	   ((unsigned long)(t1-t0)) / 1000000,
	   ((unsigned long)(t1-t0)) % 1000000,
	   ((float)NUM_REPEATS / (((unsigned long)(t1-t0))/1000000.0)));
    exit(0);
}

#endif
