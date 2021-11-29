ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
CFLAGS=-std=c18 -Wall -Werror -D_POSIX_C_SOURCE=200809L $(shell pkg-config --cflags alsa) -I$(ERL_TOP)/usr/include -D_REENTRANT -fPIC
LDLIBS=$(shell pkg-config --libs alsa) $(ERL_TOP)/usr/lib/libei.a
LDFLAGS=-shared

all: ../priv/alsa_nif.so

../priv/alsa_nif.so: alsa_nif.o
	$(CC) $^ $(LDFLAGS) $(LDLIBS) -o ../priv/alsa_nif.so

%.o : %.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o ../priv/alsa_nif.so
