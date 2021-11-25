#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

CFLAGS+=-std=c99 -Wall -Werror -O
OCFLAGS=-I/usr/local/lib/erlang/usr/include -D_REENTRANT
SOLDFLAGS=-shared
S2DLDFLAGS=-L/usr/lib/x86_64-linux-gnu -L/usr/local/lib $(shell pkg-config --libs alsa)

all: ../priv/alsa_nif.so

../priv/alsa_nif.so: alsa_nif.o
	$(CC) $(ERL_TOP)/usr/lib/libei.a alsa_nif.o $(SOLDFLAGS) $(S2DLDFLAGS) -o ../priv/alsa_nif.so

alsa_nif.o: alsa_nif.c
	$(CC) $(OCFLAGS) -fPIC -c alsa_nif.c

clean:
	rm -f *.o ../priv/alsa_nif.so
