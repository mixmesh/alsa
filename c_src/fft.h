#ifndef __FFT_H__
#define __FFT_H__

#include <stdlib.h>
#include <complex.h>

typedef enum {
    RECTANGULAR = 0,
    BARTLETT,
    HANNING,
    HAMMING,
    BLACKMAN,
    BLACKMAN_HARRIS,
    CUSTOM
} fft_win_type;

typedef struct
{
    int n;              // 2^m    
    int m;              // exponent    
    float complex* w;   // fourier kernel
    fft_win_type wt;    // window type (for resize etc)
    float c0,c1,c2,c3;  // filter constants
    float* h;           // filter factors
} fft_t;

void fft_init(fft_t* fp, size_t n);
void fft_clear(fft_t* fp);
void fft_resize(fft_t* fp, size_t n);
void fft_fft(fft_t* fp, float complex* x, float complex* y);
void fft_ifft(fft_t* fp,float complex* x);
void fft_rfft(fft_t* fp, float* x, float* y);

void fft_phase(fft_t* fp,float complex* x, float* y);
void fft_log_magnitude(fft_t* fp,float complex* x, float* y);
void fft_magnitude(fft_t* fp,float complex* x, float* y);

void fft_set_rectangular(fft_t* fp);
void fft_set_bartlett(fft_t* fp);
void fft_set_hanning(fft_t* fp);
void fft_set_hamming(fft_t* fp);
void fft_set_blackman(fft_t* fp);
void fft_set_blackman_harris(fft_t* fp);
void fft_set_custom(fft_t* fp, float c0, float c1, float c2, float c3);

#endif
