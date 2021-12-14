ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
CC=gcc	
CFLAGS= -Wall -Werror $(shell pkg-config --cflags alsa) -I$(ERL_TOP)/usr/include -D_REENTRANT -fPIC
LDLIBS=$(shell pkg-config --libs alsa) $(ERL_TOP)/usr/lib/libei.a
LDFLAGS=-shared
# -std=c18 -D_POSIX_C_SOURCE=200809L
all: ../priv/alsa_nif.so

../priv/alsa_nif.so: alsa_nif.o
	$(CC) $^ $(LDFLAGS) $(LDLIBS) -o ../priv/alsa_nif.so

%.o : %.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o ../priv/alsa_nif.so
