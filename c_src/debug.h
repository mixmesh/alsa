#ifndef __DEBUG_H__
#define __DEBUG_H__

#ifdef DEBUG
#define DEBUGF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#else
#define DEBUGF(f,a...)
#endif

#endif
