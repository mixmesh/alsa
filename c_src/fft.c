
#include <complex.h>
#include <math.h>
#include <memory.h>

#include "fft.h"

static void clear(fft_t* fp)
{
    fp->n = fp->m = 0;
    if (fp->w) {
	free(fp->w);
	fp->w = NULL;
    }
    if (fp->h) {
	free(fp->h);
	fp->h = NULL;
    }
}

//
// Reorder data according to bit numbers
// 
static void reorder(fft_t* fp, float complex* x)
{
    int j = 0;
    int i;

    for (i = 1; i < (fp->n-1); i++) {
	int k = fp->n/2;

	while(k <= j) {
	    j = j-k;
	    k = k/2;
	}
	j = j + k;
	if (i < j) {
	    float complex a = x[j];
	    x[j] = x[i];
	    x[i] = a;
	}
    }
}

//
// Given a vector of complex data compute the fft
//
static void transform(fft_t* fp, float complex* x, int inverse)
{
    int le = fp->n;
    int l;
    int windex = inverse ? -1 : 1;
    int woffs  = -windex;
    float complex* wbase = inverse ? fp->w + (fp->n-1) : fp->w;
    float complex* wptr;

    for (l = 0; l < fp->m; l++) {
	int i, j;
	int le2 = le;

	le >>= 1;

	for (i = 0; i < (int) fp->n; i += le2) {
	    float complex a = x[i] + x[i+le];
	    x[i+le] = x[i] - x[i+le];
	    x[i] = a;
	}

	wptr = wbase + windex - woffs;
	for (j = 1; j < le; j++) {
	    float complex wj = *wptr;

	    for (i = j; i < (int) fp->n; i += le2) {
		float complex a = x[i] + x[i+le];
		x[i+le] = (x[i] - x[i+le])*wj;
		x[i]    = a;
	    }
	    wptr += windex;
	}
	windex <<= 1;
    }
    reorder(fp, x);
}

static void filter(fft_t* fp, float complex* x, float complex* y)
{
    int i;

    if (!fp->h) { // as h[i] = 1.0
	if (x != y) {
	    for (i = 0; i < fp->n; i++)
		y[i] = x[i];
	}
    }
    else {
	for (i = 0; i < fp->n; i++)
	    y[i] = x[i]*fp->h[i];
    }
}

static void resize(fft_t* fp, size_t n)
{
    if (n == 0) {
	clear(fp);
    }
    else {
	int k = 1;
	int m = 0;
	int j;
	int le;
	double arg;
	double complex w;
	double complex  w0;
	float complex* xj;

	while(k < (int) n) { // k = 2^m >= n
	    k <<= 1;
	    m++;
	}
	if (k != fp->n) {
	    fp->w = realloc(fp->w, sizeof(float complex)*k);
	    fp->n = k;
	    fp->m = m;
	    le = k/2;
	    // calculate kernel w for all n
	    // [0 ... n/2-1 n/2 n/2+1 ... n-1]
	    // [0...n/2-1] is used by fft and
	    // [n-1... n/2+1] (reversed) used by inverse fft
	    arg = M_PI / le;
	    w = w0 = cos(arg)-sin(arg)*I;
	    xj = fp->w;
	    for (j = 0; j < k; j++) {
		*xj++ = (float complex) w;
		w *= w0;
	    }
	}
    }
}

//
// Create an fft instance of size n =  2^m  !!!
//
void fft_init(fft_t* fp, size_t n)
{
    memset(fp, 0, sizeof(fft_t));
    resize(fp, n);
}

void fft_clear(fft_t* fp)
{
    clear(fp);
}

static inline void win_calc(fft_t* fp)
{
    int i;

    if (fp->n == 0)
	return;
    
    switch(fp->wt) {
    case RECTANGULAR:
	if (fp->h != NULL) {
	    for (i = 0; i < (int) fp->n; i++)
		fp->h[i] = 1.0;
	}
	break;
    case BARTLETT: {
	if (fp->n == 1) {
	    fp->h[0] = 0.0;
	}
	else {
	    double arg = 2.0/(fp->n-1);
	    for (i = 0; i <= ((int)fp->n-1)/2; i++)
		fp->h[i] = i*arg;
	    for (; i < (int)fp->n; i++)
		fp->h[i] = 2.0 - i*arg;
	    break;
	}
    }
    default: {
	float c0 = fp->c0;
	float c1 = fp->c1;
	float c2 = fp->c2;
	float c3 = fp->c3;	
	if (fp->n == 1) {
	    fp->h[0] = c0+c1+c2+c3;
	}
	else {
	    double arg = 2*M_PI/(fp->n-1);
	    for (i = 0; i < fp->n; i++) {
		double ai = arg*i;
		fp->h[i] = c0 + c1*cos(ai) + c2*cos(2*ai) + c3*cos(3*ai);
	    }
	}
	break;
    }
    }
}

static inline void win_resize(fft_t* fp)
{
    fp->h = realloc(fp->h, sizeof(float)*fp->n);
    win_calc(fp);
}

void fft_resize(fft_t* fp, size_t n)
{
    resize(fp, n);
    win_resize(fp);
}

static void scale(fft_t* fp, float complex* x, float factor)
{
    int i;
    for (i = 0; i < fp->n; i++) 
	x[i] /= factor;
}

static inline void win_set(fft_t* fp, fft_win_type wt,
			   float c0, float c1, float c2, float c3)
{
    fp->wt = wt;
    fp->c0 = c0;
    fp->c1 = c1;
    fp->c2 = c2;
    fp->c3 = c3;
}

void fft_set_hamming(fft_t* fp)
{
    win_set(fp, HAMMING, 0.54,-0.46,0.0,0.0);
    win_resize(fp);
}

void fft_set_hanning(fft_t* fp)
{    
    win_set(fp, HANNING, 0.5,-0.5,0.0,0.0);
    win_resize(fp);
}

void fft_set_blackman(fft_t* fp)
{
    win_set(fp, BLACKMAN, 0.42, -0.5, 0.08, 0.0);
    win_resize(fp);
}

void fft_set_blackman_harris(fft_t* fp)
{
    win_set(fp, BLACKMAN_HARRIS, 0.35875, -0.48829, 0.14128, -0.01168);
    win_resize(fp);
}

void fft_set_rectangular(fft_t* fp)
{
    win_set(fp, RECTANGULAR, 0.0, 0.0, 0.0, 0.0);
    win_resize(fp);
}

void fft_set_bartlett(fft_t* fp)
{
    win_set(fp, BARTLETT, 0.0, 0.0, 0.0, 0.0);
    win_resize(fp);
}

void fft_set_custom(fft_t* fp, float c0, float c1, float c2, float c3)
{
    win_set(fp, CUSTOM, c0, c1, c2, c3);
    win_resize(fp);    
}

void fft_magnitude(fft_t* fp, float complex* x, float* y)
{
    int i;
    for (i = 0; i < fp->n; i++) {
	float xr = creal(x[i]);
	float xi = cimag(x[i]);	
	y[i] = xr*xr + xi*xi;
    }
}

void fft_log_magnitude(fft_t* fp, float complex* x, float* y)
{
    int i;
    for (i = 0; i < fp->n; i++) {
	float xr = crealf(x[i]);
	float xi = cimagf(x[i]);
	y[i] = 10*log10(xr*xr + xi*xi);
    }
}

void fft_phase(fft_t* fp, float complex* x, float* y)
{
    int i;
    for (i = 0; i < fp->n; i++)
	y[i] = atan2(cimag(x[i]),creal(x[i]));
}

// Run fast fourier transform (inline)
void fft_fft(fft_t* fp, complex float* x, complex float* y)
{
    filter(fp, x, y);
    transform(fp, y, 0);
}

// Real input log magnitude output (fixme, is temporary array needed?)
void fft_rfft(fft_t* fp, float* x, float* y)
{
    int i;    
    float complex* tmp = malloc(sizeof(float complex)*fp->n);

    for (i = 0; i < fp->n; i++)
	tmp[i] = CMPLXF(x[i], 0);
    filter(fp, tmp, tmp);
    transform(fp, tmp, 0);
    fft_log_magnitude(fp, tmp, y);
    free(tmp);
}

// Run inverse fast fourier transform
void fft_ifft(fft_t* fp, float complex* x)
{
    transform(fp, x, 1);
    scale(fp, x, 1.0 / fp->n);
}
