ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
CC=gcc
CFLAGS= -Wall -Werror $(shell pkg-config --cflags alsa) -I$(ERL_TOP)/usr/include -D_REENTRANT -fPIC
ALSA_LIBS=$(shell pkg-config --libs alsa) # $(ERL_TOP)/usr/lib/libei.a
NIF_SAMPLES_OBJS = alsa_samples_nif.o mix.o resample.o reformat.o
SBC_LIBS= -lsbc
SIMD_FLAGS = -msse4.1
LDFLAGS=-shared

# -std=c18 -D_POSIX_C_SOURCE=200809L
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

mix.o: mix.c
	$(CC) -O3 $(SIMD_FLAGS) $(CFLAGS)  -c $<

resample.o: resample.c
	$(CC) -O3 $(SIMD_FLAGS) $(CFLAGS)  -c $<

reformat.o: reformat.c
	$(CC) -O3 $(SIMD_FLAGS) $(CFLAGS)  -c $<

%.o: %.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o ../priv/alsa_nif.so
