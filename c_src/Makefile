ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
CC=gcc	
CFLAGS= -Wall -Werror $(shell pkg-config --cflags alsa) -I$(ERL_TOP)/usr/include -D_REENTRANT -fPIC
ALSA_LIBS=$(shell pkg-config --libs alsa) $(ERL_TOP)/usr/lib/libei.a
SBC_LIBS= -lsbc
LDFLAGS=-shared
# -std=c18 -D_POSIX_C_SOURCE=200809L
all: ../priv/alsa_nif.so ../priv/alsa_sbc_nif.so

../priv/alsa_nif.so: alsa_nif.o
	$(CC) $^ $(LDFLAGS) $(ALSA_LIBS) -o ../priv/alsa_nif.so

../priv/alsa_sbc_nif.so: alsa_sbc_nif.o
	$(CC) $^ $(LDFLAGS) $(SBC_LIBS) -o ../priv/alsa_sbc_nif.so

%.o : %.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o ../priv/alsa_nif.so
