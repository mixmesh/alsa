#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS= -fPIC -MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

CC=gcc
CFLAGS+= $(shell pkg-config --cflags alsa)
ALSA_LIBS=$(shell pkg-config --libs alsa)
NIF_SAMPLES_OBJS = alsa_samples_nif.o mix.o resample.o reformat.o wave.o filt.o
SBC_LIBS= -lsbc
SIMD_FLAGS = # -msse4.1

CFLAGS+= -Wall -Werror
LDFLAGS+= -shared

all: ../priv/alsa_nif.so \
	../priv/alsa_sbc_nif.so \
	../priv/alsa_samples_nif.so \
	../priv/mix_bench \
	../priv/reformat_bench

../priv/alsa_nif.so: alsa_nif.o
	$(CC) $^ $(LDFLAGS) $(ALSA_LIBS) -o ../priv/alsa_nif.so

../priv/alsa_sbc_nif.so: alsa_sbc_nif.o
	$(CC) $^ $(LDFLAGS) $(SBC_LIBS) -o ../priv/alsa_sbc_nif.so

../priv/alsa_samples_nif.so: $(NIF_SAMPLES_OBJS)
	$(CC) $(NIF_SAMPLES_OBJS) $(LDFLAGS) $(ALSA_LIBS) -o ../priv/alsa_samples_nif.so

../priv/mix_bench: mix.c
	$(CC) -O3 $(SIMD_FLAGS) $(CFLAGS) -DTEST -o$@ mix.c $(ALSA_LIBS)

../priv/reformat_bench: reformat.c
	$(CC) -O3 $(SIMD_FLAGS) $(CFLAGS) -DTEST -o$@ reformat.c -lm $(ALSA_LIBS)

%.o: %.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o ../priv/alsa_nif.so

.%.d:	;

-include .*.d
