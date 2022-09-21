#include <stdint.h>
#include <math.h>
#include <byteswap.h>
#include <alsa/asoundlib.h>

#define MAX_INT32_DOUBLE 2147483647.0

static int is_int_format(snd_pcm_format_t format)
{
    switch(format) {
    case SND_PCM_FORMAT_S8:
    case SND_PCM_FORMAT_U8:
    case SND_PCM_FORMAT_S16_LE:
    case SND_PCM_FORMAT_S16_BE:
    case SND_PCM_FORMAT_U16_LE:
    case SND_PCM_FORMAT_U16_BE:
    case SND_PCM_FORMAT_S24_LE:
    case SND_PCM_FORMAT_S24_BE:
    case SND_PCM_FORMAT_U24_LE:
    case SND_PCM_FORMAT_U24_BE:	
    case SND_PCM_FORMAT_S32_LE:
    case SND_PCM_FORMAT_S32_BE:
    case SND_PCM_FORMAT_U32_LE:
    case SND_PCM_FORMAT_U32_BE:
    case SND_PCM_FORMAT_A_LAW:
    case SND_PCM_FORMAT_MU_LAW:
	return 1;
    default:
	return 0;
    }
}

static int is_float_format(snd_pcm_format_t format)
{
    switch(format) {
    case SND_PCM_FORMAT_FLOAT_LE:
    case SND_PCM_FORMAT_FLOAT_BE:
    case SND_PCM_FORMAT_FLOAT64_LE:
    case SND_PCM_FORMAT_FLOAT64_BE:
	return 1;
    default:
	return 0;
    }
}

static int16_t a_law_table[256] = 
{
     -5504, -5248, -6016, -5760, -4480, -4224, -4992, -4736,
     -7552, -7296, -8064, -7808, -6528, -6272, -7040, -6784,
     -2752, -2624, -3008, -2880, -2240, -2112, -2496, -2368,
     -3776, -3648, -4032, -3904, -3264, -3136, -3520, -3392,
     -22016,-20992,-24064,-23040,-17920,-16896,-19968,-18944,
     -30208,-29184,-32256,-31232,-26112,-25088,-28160,-27136,
     -11008,-10496,-12032,-11520,-8960, -8448, -9984, -9472,
     -15104,-14592,-16128,-15616,-13056,-12544,-14080,-13568,
     -344,  -328,  -376,  -360,  -280,  -264,  -312,  -296,
     -472,  -456,  -504,  -488,  -408,  -392,  -440,  -424,
     -88,   -72,   -120,  -104,  -24,   -8,    -56,   -40,
     -216,  -200,  -248,  -232,  -152,  -136,  -184,  -168,
     -1376, -1312, -1504, -1440, -1120, -1056, -1248, -1184,
     -1888, -1824, -2016, -1952, -1632, -1568, -1760, -1696,
     -688,  -656,  -752,  -720,  -560,  -528,  -624,  -592,
     -944,  -912,  -1008, -976,  -816,  -784,  -880,  -848,
      5504,  5248,  6016,  5760,  4480,  4224,  4992,  4736,
      7552,  7296,  8064,  7808,  6528,  6272,  7040,  6784,
      2752,  2624,  3008,  2880,  2240,  2112,  2496,  2368,
      3776,  3648,  4032,  3904,  3264,  3136,  3520,  3392,
      22016, 20992, 24064, 23040, 17920, 16896, 19968, 18944,
      30208, 29184, 32256, 31232, 26112, 25088, 28160, 27136,
      11008, 10496, 12032, 11520, 8960,  8448,  9984,  9472,
      15104, 14592, 16128, 15616, 13056, 12544, 14080, 13568,
      344,   328,   376,   360,   280,   264,   312,   296,
      472,   456,   504,   488,   408,   392,   440,   424,
      88,    72,   120,   104,    24,     8,    56,    40,
      216,   200,   248,   232,   152,   136,   184,   168,
      1376,  1312,  1504,  1440,  1120,  1056,  1248,  1184,
      1888,  1824,  2016,  1952,  1632,  1568,  1760,  1696,
      688,   656,   752,   720,   560,   528,   624,   592,
      944,   912,  1008,   976,   816,   784,   880,   848
};

static int16_t mu_law_table[256] = 
{
     -32124,-31100,-30076,-29052,-28028,-27004,-25980,-24956,
     -23932,-22908,-21884,-20860,-19836,-18812,-17788,-16764,
     -15996,-15484,-14972,-14460,-13948,-13436,-12924,-12412,
     -11900,-11388,-10876,-10364, -9852, -9340, -8828, -8316,
      -7932, -7676, -7420, -7164, -6908, -6652, -6396, -6140,
      -5884, -5628, -5372, -5116, -4860, -4604, -4348, -4092,
      -3900, -3772, -3644, -3516, -3388, -3260, -3132, -3004,
      -2876, -2748, -2620, -2492, -2364, -2236, -2108, -1980,
      -1884, -1820, -1756, -1692, -1628, -1564, -1500, -1436,
      -1372, -1308, -1244, -1180, -1116, -1052,  -988,  -924,
       -876,  -844,  -812,  -780,  -748,  -716,  -684,  -652,
       -620,  -588,  -556,  -524,  -492,  -460,  -428,  -396,
       -372,  -356,  -340,  -324,  -308,  -292,  -276,  -260,
       -244,  -228,  -212,  -196,  -180,  -164,  -148,  -132,
       -120,  -112,  -104,   -96,   -88,   -80,   -72,   -64,
        -56,   -48,   -40,   -32,   -24,   -16,    -8,     -1,
      32124, 31100, 30076, 29052, 28028, 27004, 25980, 24956,
      23932, 22908, 21884, 20860, 19836, 18812, 17788, 16764,
      15996, 15484, 14972, 14460, 13948, 13436, 12924, 12412,
      11900, 11388, 10876, 10364,  9852,  9340,  8828,  8316,
       7932,  7676,  7420,  7164,  6908,  6652,  6396,  6140,
       5884,  5628,  5372,  5116,  4860,  4604,  4348,  4092,
       3900,  3772,  3644,  3516,  3388,  3260,  3132,  3004,
       2876,  2748,  2620,  2492,  2364,  2236,  2108,  1980,
       1884,  1820,  1756,  1692,  1628,  1564,  1500,  1436,
       1372,  1308,  1244,  1180,  1116,  1052,   988,   924,
        876,   844,   812,   780,   748,   716,   684,   652,
        620,   588,   556,   524,   492,   460,   428,   396,
        372,   356,   340,   324,   308,   292,   276,   260,
        244,   228,   212,   196,   180,   164,   148,   132,
        120,   112,   104,    96,    88,    80,    72,    64,
         56,    48,    40,    32,    24,    16,     8,     0
};

// convert signed 24 bit into signed 32 bit
static inline int32_t i24_i32(uint32_t x)
{
    return (((int32_t)x) << 8);
}

// convert unsigned 24 bit into signed 32 bit
static inline int32_t u24_i32(uint32_t x)
{
    return (((int32_t)(x & 0xffffff)) - 0x800000) << 8;
}

// convert unsigned 16 bit into signed 32 bit
static inline int32_t u16_i32(uint16_t x)
{
    return ((int32_t)x - 0x800000) << 8;
}

static inline uint32_t i32_i24(int32_t x)
{
    return ((uint32_t)x) >> 8;
}

static inline int16_t i32_i16(int32_t x)
{
    return (x >> 16);
}

static inline int8_t i32_i8(int32_t x)
{
    return (x >> 24);
}


static inline uint32_t i32_u24(int32_t x)
{
    return ((x >> 8) + 0x800000) & 0xffffff;
}

// read pcm sample and return as 32 bit signed integer
static inline int32_t read_pcm_int(snd_pcm_format_t format, int8_t* ptr)
{
    switch(format) {
    case SND_PCM_FORMAT_S8:
	return (int32_t) *((int8_t*)ptr) << 24;
    case SND_PCM_FORMAT_U8:
	return (int32_t)(*((uint8_t*)ptr)-0x80) << 24;
    case SND_PCM_FORMAT_S16_LE:
	return (int32_t) le16toh(*((uint16_t*)ptr)) << 16;
    case SND_PCM_FORMAT_S16_BE:
	return ((int32_t) be16toh(*((uint16_t*)ptr))) << 16;
    case SND_PCM_FORMAT_U16_LE:
	return ((int32_t)(le16toh(*((uint16_t*)ptr))-0x8000)) << 16;
    case SND_PCM_FORMAT_U16_BE:
	return ((int32_t)(be16toh(*((uint16_t*)ptr))-0x8000)) << 16;
    case SND_PCM_FORMAT_S24_LE:
	return i24_i32(le32toh(*((uint32_t*)ptr)));
    case SND_PCM_FORMAT_S24_BE:
	return i24_i32(be32toh(*((uint32_t*)ptr)));
    case SND_PCM_FORMAT_U24_LE:
	return u24_i32(le32toh(*((uint32_t*)ptr)));
    case SND_PCM_FORMAT_U24_BE:
	return u24_i32(be32toh(*((uint32_t*)ptr)));
    case SND_PCM_FORMAT_S32_LE:
	return (int32_t) le32toh(*((uint32_t*)ptr));
    case SND_PCM_FORMAT_S32_BE:
	return (int32_t) be32toh(*((uint32_t*)ptr));
    case SND_PCM_FORMAT_U32_LE:
	return (int32_t) le32toh(*((uint32_t*)ptr))-0x80000000;
    case SND_PCM_FORMAT_U32_BE:
	return (int32_t) be32toh(*((uint32_t*)ptr))-0x80000000;
    case SND_PCM_FORMAT_A_LAW:
	return ((int32_t) a_law_table[*((uint8_t*)ptr)]) << 16;
    case SND_PCM_FORMAT_MU_LAW:
	return ((int32_t) mu_law_table[*((uint8_t*)ptr)]) << 16;
    case SND_PCM_FORMAT_FLOAT_LE: {
	union { float f; uint32_t u; } fu;
	fu.u = le32toh(*((uint32_t*)ptr));
	return (int32_t) roundf(fu.f*MAX_INT32_DOUBLE);
    }
    case SND_PCM_FORMAT_FLOAT_BE: {
	union { float f; uint32_t u; } fu;
	fu.u = be32toh(*((uint32_t*)ptr));
	return (int32_t) roundf(fu.f*MAX_INT32_DOUBLE);	
    }
    case SND_PCM_FORMAT_FLOAT64_LE: {
	union { double d; uint64_t u; } du;
	du.u = le64toh(*((uint64_t*)ptr));
	return (int32_t) round(du.d*MAX_INT32_DOUBLE);
    }
    case SND_PCM_FORMAT_FLOAT64_BE: {
	union { double d; uint64_t u; } du;
	du.u = be64toh(*((uint64_t*)ptr));
	return (int32_t) round(du.d*MAX_INT32_DOUBLE);
    }	
    default:
	return 0;
    }
}


static uint8_t mu_law_compress_table[256] = 
{
     0,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,
     4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
     5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
     5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
};

static inline uint8_t mu_law_encode(int16_t x0)
{
    uint8_t sign = (x0 < 0) ? 0x80 : 0x00;
    int16_t ax = abs(x0);
    int16_t x  = ((ax < 32635) ? ax : 32635) + 0x84;
    uint8_t e = mu_law_compress_table[(x >> 7) & 0xff];
    uint8_t m = (x >> (e+3)) & 0xf;
    return ~(sign | (e << 4) | m); //  & 0xff;
}

static int a_law_compress_table[128] = 
{
     1,1,2,2,3,3,3,3,
     4,4,4,4,4,4,4,4,
     5,5,5,5,5,5,5,5,
     5,5,5,5,5,5,5,5,
     6,6,6,6,6,6,6,6,
     6,6,6,6,6,6,6,6,
     6,6,6,6,6,6,6,6,
     6,6,6,6,6,6,6,6,
     7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7
};

static inline uint8_t a_law_encode(int16_t x0)
{
    uint8_t sign = ((~x0 >> 8) & 0x80) ^ 0x55;
    int16_t ax = abs(x0);
    int16_t x = ((ax < 32635) ? ax : 32635);
    if (x >= 256) {
	int e = a_law_compress_table[(x >> 8) & 0x7f];
	int m = (x >> (e+3)) & 0xf;
	return ((e << 4) | m) ^ sign;
    }
    else
	return ((x >> 4) & 0xff) ^ sign;
}

static inline void write_pcm_int(snd_pcm_format_t format, int32_t val,
				 int8_t* ptr)
{
    switch(format) {
    case SND_PCM_FORMAT_S8:
	*((int8_t*)ptr) = (int8_t) (val>>24);
	break;
    case SND_PCM_FORMAT_U8:
	*((uint8_t*)ptr) = (((int8_t)(val>>24) + 0x80));
	break;
    case SND_PCM_FORMAT_S16_LE:
	*((uint16_t*)ptr) = htole16((int16_t) (val>>16));
	break;
    case SND_PCM_FORMAT_S16_BE:
	*((uint16_t*)ptr) = htobe16((int16_t) (val>>16));
	break;
    case SND_PCM_FORMAT_U16_LE:
	*((uint16_t*)ptr) = htole16((int16_t) (val>>16) + 0x8000);
	break;
    case SND_PCM_FORMAT_U16_BE:
	*((uint16_t*)ptr) = htobe16((int16_t) (val>>16) + 0x8000);
	break;
    case SND_PCM_FORMAT_S24_LE:
	*((uint32_t*)ptr) = htole32(i32_i24(val));
	break;
    case SND_PCM_FORMAT_S24_BE:
	*((uint32_t*)ptr) = htobe32(i32_i24(val));
	break;
    case SND_PCM_FORMAT_U24_LE:
	*((uint32_t*)ptr) = htole32(i32_u24(val));
	break;
    case SND_PCM_FORMAT_U24_BE:
	*((uint32_t*)ptr) = htobe32(i32_u24(val));
	break;	
    case SND_PCM_FORMAT_S32_LE:
	*((uint32_t*)ptr) = htole32((int32_t) val);
	break;
    case SND_PCM_FORMAT_S32_BE:
	*((uint32_t*)ptr) = htobe32((int32_t) val);
	break;
    case SND_PCM_FORMAT_U32_LE:
	*((uint32_t*)ptr) = htole32((int32_t)val + 0x80000000);
	break;
    case SND_PCM_FORMAT_U32_BE:
	*((uint32_t*)ptr) = htobe32((int32_t)val + 0x80000000);
	break;
    case SND_PCM_FORMAT_A_LAW:
	*((uint8_t*) ptr) = a_law_encode(val >> 16);
	break;
    case SND_PCM_FORMAT_MU_LAW:
	*((uint8_t*) ptr) = mu_law_encode(val >> 16);
	break;
    case SND_PCM_FORMAT_FLOAT_LE: {
	union { float f; uint32_t u; } fu;
	fu.f = (val / MAX_INT32_DOUBLE);
	*((uint32_t*)ptr) = htole32(fu.u);
	break;
    }
    case SND_PCM_FORMAT_FLOAT_BE: {
	union { float f; uint32_t u; } fu;
	fu.f = (val / MAX_INT32_DOUBLE);
	*((uint32_t*)ptr) = htobe32(fu.u);
	break;	
    }
    case SND_PCM_FORMAT_FLOAT64_LE: {
	union { double d; uint64_t u; } du;
	du.d = (val / MAX_INT32_DOUBLE);
	*((uint64_t*)ptr) = htole64(du.u);
	break;
    }
    case SND_PCM_FORMAT_FLOAT64_BE: {
	union { double d; uint64_t u; } du;
	du.d = (val / MAX_INT32_DOUBLE);
	*((uint64_t*)ptr) = htobe64(du.u);
	break;	
    }	
    default:
	break;
    }
}

static inline double read_pcm_float(snd_pcm_format_t format, int8_t* ptr)
{
    switch(format) {
    case SND_PCM_FORMAT_FLOAT_LE: {
	union { float f; uint32_t u; } fu;
	fu.u = le32toh(*((uint32_t*)ptr));
	return fu.f;
    }
    case SND_PCM_FORMAT_FLOAT_BE: {
	union { float f; uint32_t u; } fu;
	fu.u = be32toh(*((uint32_t*)ptr));
	return fu.f;
    }
    case SND_PCM_FORMAT_FLOAT64_LE: {
	union { double d; uint64_t u; } du;
	du.u = le64toh(*((uint64_t*)ptr));
	return du.d;
    }
    case SND_PCM_FORMAT_FLOAT64_BE: {
	union { double d; uint64_t u; } du;
	du.u = be64toh(*((uint64_t*)ptr));
	return du.d;
    }
    default: {
	int32_t val = read_pcm_int(format, ptr);
	return (val / MAX_INT32_DOUBLE);
    }
    }
}

static inline void write_pcm_float(snd_pcm_format_t format,double d,
				   int8_t* ptr)
{
    switch(format) {
    case SND_PCM_FORMAT_FLOAT_LE: {
	union { float f; uint32_t u; } fu;
	fu.f = d;
	*((uint32_t*)ptr) = htole32(fu.u);
	break;
    }
    case SND_PCM_FORMAT_FLOAT_BE: {
	union { float f; uint32_t u; } fu;
	fu.f = d;
	*((uint32_t*)ptr) = htobe32(fu.u);
	break;	
    }
    case SND_PCM_FORMAT_FLOAT64_LE: {
	union { double d; uint64_t u; } du;
	du.d = d;
	*((uint64_t*)ptr) = htole64(du.u);
	break;
    }
    case SND_PCM_FORMAT_FLOAT64_BE: {
	union { double d; uint64_t u; } du;
	du.d = d;
	*((uint64_t*)ptr) = htobe64(du.u);
	break;	
    }
    default:
	write_pcm_int(format, (int32_t) round(d*MAX_INT32_DOUBLE), ptr);
	break;
    }
}

// ASSUMED map (fixme, profile)
// FL = FRONT LEFT
// FR = FRONT RIGHT
// FC = FRONT CENTER
// SL = SURRROUND LEFT
// SR = SURROUND RIGHT
// RL = REAR LEFT
// RR = REAR RIGHT
// LFE = SUB WOOFER

// 1: FC
// 20: FL, FR
// 21: FL, FR, LFE
// 40: FL, FR, RL, RR
// 41: FL, FR, LFE, RL, RR
// 51: FL, FR, FC,  LFE, RL, RR
// 61: FL, FR, LFE, RL, RR, SL, SR
// 71: FL, FR, FC, LFE, RL, RR, SL, SR

// 1: FC

#define CMASK   0xf000
#define CCHAN   0x1000
#define CADD    0x2000
#define CRES    0x4000
#define CCONST  0x8000
#define CCONST1 0xc000

#define CHAN(x)  (CCHAN|((x) & 0xf))
#define ADD(x,y) (CADD|((x) & 0xf)|(((y) & 0xf)<<4))
#define CONST(c) (CCONST|((c & 0x7fff)))

typedef uint16_t map_t;

// destination channels = 0 (empty)
static map_t chan_map_00_00[0] = { };
static map_t chan_map_00_10[0] = { };
static map_t chan_map_00_20[0] = { };
static map_t chan_map_00_21[0] = { };
static map_t chan_map_00_40[0] = { };
static map_t chan_map_00_41[0] = { };
static map_t chan_map_00_51[0] = { };
static map_t chan_map_00_61[0] = { };
static map_t chan_map_00_71[0] = { };

// destination channels = 1 mono
static map_t chan_map_10_00[1] = { CONST(0)  };
static map_t chan_map_10_10[1] = { CHAN(0)  };
static map_t chan_map_10_20[1] = { ADD(0,1) };
static map_t chan_map_10_21[1] = { ADD(0,1) };
static map_t chan_map_10_40[1] = { ADD(0,1) };
static map_t chan_map_10_41[1] = { ADD(0,1) };
static map_t chan_map_10_51[1] = { ADD(0,1) };
static map_t chan_map_10_61[1] = { ADD(0,1) };
static map_t chan_map_10_71[1] = { ADD(0,1) };

// destination channels = 2 stereo
static map_t chan_map_20_00[2] = { CONST(0), CONST(0) };
static map_t chan_map_20_10[2] = { CHAN(0), CHAN(0)  };
static map_t chan_map_20_20[2] = { CHAN(0), CHAN(1) };
static map_t chan_map_20_21[2] = { CHAN(0), CHAN(1) };
static map_t chan_map_20_40[2] = { CHAN(0), CHAN(1) };
static map_t chan_map_20_41[2] = { CHAN(0), CHAN(1) };
static map_t chan_map_20_51[2] = { CHAN(0), CHAN(1) };
static map_t chan_map_20_61[2] = { CHAN(0), CHAN(1) };
static map_t chan_map_20_71[2] = { CHAN(0), CHAN(1) };

// destination channels = 3 (2.1) stereo + lfe
static map_t chan_map_21_00[3] = { CONST(0), CONST(0), CONST(0) };
static map_t chan_map_21_10[3] = { CHAN(0), CHAN(0), CONST(0) };
static map_t chan_map_21_20[3] = { CHAN(0), CHAN(1), CONST(0) };
static map_t chan_map_21_21[3] = { CHAN(0), CHAN(1), CHAN(2) };
static map_t chan_map_21_40[3] = { CHAN(0), CHAN(1), CONST(0) };
static map_t chan_map_21_41[3] = { CHAN(0), CHAN(1), CHAN(2) };
static map_t chan_map_21_51[3] = { CHAN(0), CHAN(1), CHAN(3)  };
static map_t chan_map_21_61[3] = { CHAN(0), CHAN(1), CHAN(2) };
static map_t chan_map_21_71[3] = { CHAN(0), CHAN(1), CHAN(3)  };

// destination channels = 4 (4.0) front + rear
static map_t chan_map_40_00[4] = { CONST(0), CONST(0), CONST(0), CONST(0) };
static map_t chan_map_40_10[4] = { CHAN(0), CHAN(0), CHAN(0), CHAN(0) };
static map_t chan_map_40_20[4] = { CHAN(0), CHAN(1), CHAN(0), CHAN(1) };
static map_t chan_map_40_21[4] = { CHAN(0), CHAN(1), CHAN(0), CHAN(0) };
static map_t chan_map_40_40[4] = { CHAN(0), CHAN(1), CHAN(2), CHAN(3) };
static map_t chan_map_40_41[4] = { CHAN(0), CHAN(1), CHAN(3), CHAN(4) };
static map_t chan_map_40_51[4] = { CHAN(0), CHAN(1), CHAN(4), CHAN(5) };
static map_t chan_map_40_61[4] = { CHAN(0), CHAN(1), CHAN(3), CHAN(4) };
static map_t chan_map_40_71[4] = { CHAN(0), CHAN(1), CHAN(4), CHAN(5) };

// destination channels = 5 (4.1) front + lfe + rear
static map_t chan_map_41_00[5] = { CONST(0), CONST(0), CONST(0), CONST(0), CONST(0) };
static map_t chan_map_41_10[5] = { CHAN(0), CHAN(0), CONST(0), CHAN(0), CHAN(0) };
static map_t chan_map_41_20[5] = { CHAN(0), CHAN(1), CONST(0), CHAN(0), CHAN(1) };
static map_t chan_map_41_21[5] = { CHAN(0), CHAN(1), CHAN(2),  CHAN(0), CHAN(1) };
static map_t chan_map_41_40[5] = { CHAN(0), CHAN(1), CONST(0), CHAN(2), CHAN(3) };
static map_t chan_map_41_41[5] = { CHAN(0), CHAN(1), CHAN(2),  CHAN(3), CHAN(4) };
static map_t chan_map_41_51[5] = { CHAN(0), CHAN(1), CHAN(3),  CHAN(4), CHAN(5) };
static map_t chan_map_41_61[5] = { CHAN(0), CHAN(1), CHAN(2),  CHAN(3), CHAN(4) };
static map_t chan_map_41_71[5] = { CHAN(0), CHAN(1), CHAN(3),  CHAN(4), CHAN(5) };

// destination channels = 6 (5.1) front + center + lfe + rear
static map_t chan_map_51_00[6] = { CONST(0), CONST(0), CONST(0), CONST(0), CONST(0), CONST(0) };
static map_t chan_map_51_10[6] = { CHAN(0), CHAN(0), CONST(0), CHAN(0), CHAN(0), CHAN(0) };
static map_t chan_map_51_20[6] = { CHAN(0), CHAN(1), ADD(0,1), CONST(0), CHAN(0), CHAN(1) };
static map_t chan_map_51_21[6] = { CHAN(0), CHAN(1), ADD(0,1), CHAN(2), CHAN(0), CHAN(1) };
static map_t chan_map_51_40[6] = { CHAN(0), CHAN(1), ADD(0,1), CONST(0), CHAN(2), CHAN(3) };
static map_t chan_map_51_41[6] = { CHAN(0), CHAN(1), ADD(0,1), CHAN(2),  CHAN(3), CHAN(4) };
static map_t chan_map_51_51[6] = { CHAN(0), CHAN(1), CHAN(2),  CHAN(3), CHAN(4), CHAN(5) };
static map_t chan_map_51_61[6] = { CHAN(0), CHAN(1), ADD(0,1), CHAN(2),  CHAN(3), CHAN(4) };
static map_t chan_map_51_71[6] = { CHAN(0), CHAN(1), CHAN(2),  CHAN(3), CHAN(4), CHAN(5) };

// destination channels = 7 (6.1) front + lfe + rear + surround
static map_t chan_map_61_00[7] = { CONST(0), CONST(0), CONST(0), CONST(0), CONST(0), CONST(0), CONST(0) };
static map_t chan_map_61_10[7] = { CHAN(0), CHAN(0), CONST(0), CHAN(0), CHAN(0), CHAN(0), CHAN(0) };
static map_t chan_map_61_20[7] = { CHAN(0), CHAN(1), CONST(0), CHAN(0), CHAN(1), CHAN(0), CHAN(1) };
static map_t chan_map_61_21[7] = { CHAN(0), CHAN(1), CHAN(2),  CHAN(0), CHAN(1), CHAN(0), CHAN(1) };
static map_t chan_map_61_40[7] = { CHAN(0), CHAN(1), CONST(0), CHAN(2), CHAN(3), CHAN(2), CHAN(3) };
static map_t chan_map_61_41[7] = { CHAN(0), CHAN(1), CHAN(2),  CHAN(3), CHAN(4), CHAN(3), CHAN(4) };
static map_t chan_map_61_51[7] = { CHAN(0), CHAN(1), CHAN(3),  CHAN(4), CHAN(5), CHAN(4), CHAN(5) };
static map_t chan_map_61_61[7] = { CHAN(0), CHAN(1), CHAN(2),  CHAN(3), CHAN(4), CHAN(5), CHAN(6) };
static map_t chan_map_61_71[7] = { CHAN(0), CHAN(1), CHAN(3),  CHAN(4), CHAN(5), CHAN(6), CHAN(7) };

// destination channels = 8 (7.1) front + center + lfe + rear + surround
static map_t chan_map_71_00[8] = { CONST(0), CONST(0), CONST(0), CONST(0), CONST(0), CONST(0), CONST(0), CONST(0) };
static map_t chan_map_71_10[8] = { CHAN(0), CHAN(0), CHAN(0),  CONST(0), CHAN(0), CHAN(0), CHAN(0), CHAN(0) };
static map_t chan_map_71_20[8] = { CHAN(0), CHAN(1), ADD(0,1), CONST(0), CHAN(0), CHAN(1), CHAN(0), CHAN(1) };
static map_t chan_map_71_21[8] = { CHAN(0), CHAN(1), ADD(0,1), CONST(0), CHAN(0), CHAN(1), CHAN(0), CHAN(1) };
static map_t chan_map_71_40[8] = { CHAN(0), CHAN(1), ADD(0,1), CHAN(2), CHAN(0), CHAN(1), CHAN(0), CHAN(1) };
static map_t chan_map_71_41[8] = { CHAN(0), CHAN(1), ADD(0,1), CONST(0), CHAN(2), CHAN(3), CHAN(2), CHAN(3) };
static map_t chan_map_71_51[8] = { CHAN(0), CHAN(1), ADD(0,1), CHAN(2), CHAN(3), CHAN(4), CHAN(3), CHAN(4) };
static map_t chan_map_71_61[8] = { CHAN(0), CHAN(1), ADD(0,1), CHAN(2), CHAN(3), CHAN(4), CHAN(5), CHAN(6) };
static map_t chan_map_71_71[8] = { CHAN(0), CHAN(1), CHAN(2), CHAN(3), CHAN(4), CHAN(5), CHAN(6), CHAN(7) };

// chan_map[dst][src]
static map_t* chan_map[9][9] =
{ { chan_map_00_00, chan_map_00_10, chan_map_00_20, chan_map_00_21, chan_map_00_40, chan_map_00_41, chan_map_00_51, chan_map_00_61, chan_map_00_71 },
  { chan_map_10_00, chan_map_10_10, chan_map_10_20, chan_map_10_21, chan_map_10_40, chan_map_10_41, chan_map_10_51, chan_map_10_61, chan_map_10_71 },
  { chan_map_20_00, chan_map_20_10, chan_map_20_20, chan_map_20_21, chan_map_20_40, chan_map_20_41, chan_map_20_51, chan_map_20_61, chan_map_20_71 },
  { chan_map_21_00, chan_map_21_10, chan_map_21_20, chan_map_21_21, chan_map_21_40, chan_map_21_41, chan_map_21_51, chan_map_21_61, chan_map_21_71 },
  { chan_map_40_00, chan_map_40_10, chan_map_40_20, chan_map_40_21, chan_map_40_40, chan_map_40_41, chan_map_40_51, chan_map_40_61, chan_map_40_71 },
  { chan_map_41_00, chan_map_41_10, chan_map_41_20, chan_map_41_21, chan_map_41_40, chan_map_41_41, chan_map_41_51, chan_map_41_61, chan_map_41_71 },
  { chan_map_51_00, chan_map_51_10, chan_map_51_20, chan_map_51_21, chan_map_51_40, chan_map_51_41, chan_map_51_51, chan_map_51_61, chan_map_51_71 },
  { chan_map_61_00, chan_map_61_10, chan_map_61_20, chan_map_61_21, chan_map_61_40, chan_map_61_41, chan_map_61_51, chan_map_61_61, chan_map_61_71 },
  { chan_map_71_00, chan_map_71_10, chan_map_71_20, chan_map_71_21, chan_map_71_40, chan_map_71_41, chan_map_71_51, chan_map_71_61, chan_map_71_71 }
};



static inline int32_t sadd_int(int32_t a, int32_t b)
{
    int64_t c = a;
    c += b;
    if (c < -0x7fffffff) return -0x7fffffff;
    if (c >  0x7fffffff) return  0x7fffffff;
    return c;
}

static inline void map_frame_int(int32_t* src_frame, size_t src_channels,
				 int32_t* dst_frame, size_t dst_channels)
{
    map_t* map = chan_map[dst_channels][src_channels];
    int i;
    
    for (i = 0; i < dst_channels; i++) {
	map_t e = map[i];
	switch(e & CMASK) {
	case CCHAN:
	    dst_frame[i] = src_frame[e & 0xf];
	    break;
	case CADD:
	    dst_frame[i] = sadd_int(src_frame[e & 0xf],src_frame[(e>>4) & 0xf]);
	    break;
	case CCONST:
	case CCONST1: {  // constant
	    int32_t val = (((int16_t)((e & 0x7fff) << 1)) >> 1) << 16;
	    dst_frame[i] = val;
	    break;
	}  
	default:
	    break;
	}
    }
}

static inline double sadd_float(double a, double b)
{
    double c = a+b;
    if (c < -1.0) return -1.0;
    if (c >  1.0) return  1.0;
    return c;
}

static inline void map_frame_float(double* src_frame, size_t src_channels,
				   double* dst_frame, size_t dst_channels)
{
    map_t* map = chan_map[dst_channels][src_channels];
    int i;
    
    for (i = 0; i < dst_channels; i++) {
	map_t e = map[i];
	switch(e & CMASK) {
	case CCHAN:
	    dst_frame[i] = src_frame[e & 0xf];
	    break;
	case CADD:
	    dst_frame[i] = sadd_float(src_frame[e & 0xf],
				      src_frame[(e>>4) & 0xf]);
	    break;
	case CCONST:
	case CCONST1: {  // constant
	    int32_t val = (((int16_t)((e & 0x7fff) << 1)) >> 1) << 16;
	    dst_frame[i] = val / MAX_INT32_DOUBLE;
	    break;
	}
	default:
	    break;
	}
    }
}

// dst_format is integer based
void reformat_int(snd_pcm_format_t src_format, size_t src_channels,
		  int8_t* src,
		  snd_pcm_format_t dst_format, size_t dst_channels,
		  int8_t* dst,
		  size_t n)
{
    int i;
    ssize_t src_size = snd_pcm_format_size(src_format, 1);
    ssize_t dst_size = snd_pcm_format_size(dst_format, 1);
    
    for (i = 0; i < n; i++) {
	int32_t src_frame[src_channels];
	int j;
	
	for (j = 0; j < src_channels; j++) {
	    src_frame[j] = read_pcm_int(src_format, src);
	    src += src_size;
	}
	if (dst_channels == src_channels) {
	    for (j = 0; j < dst_channels; j++) {
		write_pcm_int(dst_format, src_frame[j], dst);
		dst += dst_size;
	    }
	}
	else {
	    int32_t dst_frame[dst_channels];
	    map_frame_int(src_frame, src_channels,
			  dst_frame, dst_channels);
	    for (j = 0; j < dst_channels; j++) {
		write_pcm_int(dst_format, dst_frame[j], dst);
		dst += dst_size;
	    }
	}
    }
}

// dst_format is float based
void reformat_float(snd_pcm_format_t src_format, size_t src_channels,
		    int8_t* src,
		    snd_pcm_format_t dst_format, size_t dst_channels,
		    int8_t* dst,
		    size_t n)
{
    int i;
    ssize_t src_size = snd_pcm_format_size(src_format, 1);
    ssize_t dst_size = snd_pcm_format_size(dst_format, 1);

    for (i = 0; i < n; i++) {
	double src_frame[src_channels];
	int j;
	
	for (j = 0; j < src_channels; j++) {
	    src_frame[j] = read_pcm_float(src_format, src);
	    src += src_size;
	}
	if (dst_channels == src_channels) {
	    for (j = 0; j < dst_channels; j++) {
		write_pcm_float(dst_format, src_frame[j], dst);
		dst += dst_size;
	    }
	}
	else {
	    double dst_frame[dst_channels];
	    map_frame_float(src_frame, src_channels,
			    dst_frame, dst_channels);
	    for (j = 0; j < dst_channels; j++) {
		write_pcm_float(dst_format, dst_frame[j], dst);
		dst += dst_size;
	    }
	}
    }
}


// reformat src samples in src_format into dst samples in dst_format
void reformat(snd_pcm_format_t src_format, size_t src_channels, void* src,
	      snd_pcm_format_t dst_format, size_t dst_channels, void* dst,
	      size_t n)
{
    if (is_int_format(dst_format)) {
	reformat_int(src_format, src_channels, src,
		     dst_format, dst_channels, dst,
		     n);
    }

    else if (is_float_format(dst_format)) {
	reformat_float(src_format, src_channels, src,
		       dst_format, dst_channels, dst,
		       n);	
    }
}

#ifdef TEST

// #define HARD_DEBUG
#define NUM_FRAMES   1024
#define SRC_CHANNELS 3
#define DST_CHANNELS 6
#define BUF_SIZE     (sizeof(int16_t)*NUM_FRAMES*NUM_CHANNELS)
#define NUM_REPEATS  10000
#if __BYTE_ORDER == __LITTLE_ENDIAN
#define SRC_FORMAT   SND_PCM_FORMAT_S16_LE
#define DST_FORMAT   SND_PCM_FORMAT_FLOAT_LE
#else
#define SRC_FORMAT   SND_PCM_FORMAT_S16_BE
#define DST_FORMAT   SND_PCM_FORMAT_FLOAT_BE
#endif

typedef int16_t src_type_t;
typedef int32_t dst_type_t;

#include <sys/time.h>
uint64_t time_tick(void)
{
    struct timeval t;
    gettimeofday(&t, 0);
    return t.tv_sec*(uint64_t)1000000 + t.tv_usec;
}

// transform 
void test_reformat(src_type_t* src, dst_type_t* dst, size_t nframes,
		   size_t loop)
{
    while(loop--) {
	reformat(SRC_FORMAT,SRC_CHANNELS,src,
		 DST_FORMAT,DST_CHANNELS,dst,
		 nframes);
    }
}

// print one frame of data
void print_frame(snd_pcm_format_t format, size_t channels, void* data)
{
    int8_t* ptr = (int8_t*) data;
    ssize_t sample_size = snd_pcm_format_size(format, 1);
    int i = 0;
    if (is_int_format(format)) {
	int32_t sample = read_pcm_int(format, ptr);
	printf("[%d]=%d", i++, sample);
	ptr += sample_size;	
	channels--;
	while(channels--) {	    
	    sample = read_pcm_int(format, ptr);
	    printf(",[%d]=%d", i++, sample);
	    ptr += sample_size;
	}
    }
    else if (is_float_format(format)) {
	double sample = read_pcm_float(format, ptr);
	printf("[%d]=%f", i++, sample);
	ptr += sample_size;	
	channels--;
	while(channels--) {	    
	    sample = read_pcm_float(format, ptr);
	    printf(",[%d]=%f", i++, sample);
	    ptr += sample_size;
	}
    }
}

void validate_reformat(src_type_t* src, dst_type_t* dst,
		       src_type_t* res, size_t n)
{
    int si, di, i;

#ifdef HARD_DEBUG
    printf("src:");
    print_frame(SRC_FORMAT, SRC_CHANNELS, src); printf("\n");
#endif

    reformat(SRC_FORMAT,SRC_CHANNELS,src, DST_FORMAT,DST_CHANNELS,dst,n);
    
#ifdef HARD_DEBUG    
    printf("dst:");
    print_frame(DST_FORMAT, DST_CHANNELS, dst);  printf("\n");
#endif

    reformat(DST_FORMAT,DST_CHANNELS,dst,SRC_FORMAT,SRC_CHANNELS,res,n);

#ifdef HARD_DEBUG    
    printf("res:");
    print_frame(SRC_FORMAT, SRC_CHANNELS, res); printf("\n");
#endif
    
    di = 0;
    si = 0;
    for (i = 0; i < n; i++) {
	if ((src[si] != res[si]) ||
	    (src[si+1] != res[si+1]) ||
	    (src[si+2] != res[si+2])) {
	    printf("error: frame %d not formated\n", si);
	    printf("src:");
	    print_frame(SRC_FORMAT, SRC_CHANNELS, &src[si]); printf("\n");
	    printf("res:");
	    print_frame(SRC_FORMAT, SRC_CHANNELS, &res[si]); printf("\n");
	    printf("dst:");
	    print_frame(DST_FORMAT, DST_CHANNELS, &dst[di]);  printf("\n");
	    exit(1);
	}
	si += SRC_CHANNELS;
	di += DST_CHANNELS;
    }
}


void test_convert(int16_t in)
{
    int8_t data[4];
    double d;
    int32_t x, y;
    int16_t out;
    x = in << 16;
    printf("in=%d, x=%d\n", in, x);
    write_pcm_int(SND_PCM_FORMAT_FLOAT_LE, x, data);
    d = read_pcm_float(SND_PCM_FORMAT_FLOAT_LE, data);
    printf("d=%f, d32=%f\n", d, d*MAX_INT32_DOUBLE);
    write_pcm_float(SND_PCM_FORMAT_S16_LE, d, data);
    y = read_pcm_int(SND_PCM_FORMAT_S16_LE, data);
    out = y >> 16;
    printf("y=%d, out=%d\n", y, out);
}

int main(int argc, char** argv)
{
    src_type_t  src[NUM_FRAMES*SRC_CHANNELS];
    src_type_t  res[NUM_FRAMES*SRC_CHANNELS];
    dst_type_t  dst[NUM_FRAMES*DST_CHANNELS];
    int i;
    uint64_t t0, t1;

#ifdef HARD_DEBUG
    test_convert(10);
    test_convert(-8);
    test_convert(124);
#endif

    for (i = 0; i < NUM_FRAMES*SRC_CHANNELS; i+= SRC_CHANNELS) {
	src[i] = 10;
	src[i+1] = -8;
	src[i+2] = 124;
    }
    validate_reformat(src, dst, res, NUM_FRAMES);
    
    t0 = time_tick();
    test_reformat(src, dst, NUM_FRAMES, NUM_REPEATS);
    t1 = time_tick();
    printf("time = %lus+%luus, %f reformat/s\n",
	   ((unsigned long)(t1-t0)) / 1000000,
	   ((unsigned long)(t1-t0)) % 1000000,
	   ((float)NUM_REPEATS / (((unsigned long)(t1-t0))/1000000.0)));
    exit(0);
}

#endif
