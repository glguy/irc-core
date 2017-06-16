.PHONY: help clean macos linux default

UNAME:=$(shell uname -s)

ifeq ($(UNAME),Darwin)
default: macos
else ifeq ($(UNAME),Linux)
default: linux
else
default: help
endif

help:
	@echo 'Currently this Makefile only autodetects Linux and Darwin'
	@echo 'You can force a specific build with "make macos" or "make linux"'

macos: glirc-otr.dylib
linux:  glirc-otr.so

glirc-otr.dylib: glirc-otr.c
	cc -shared -o $@ $^ \
	  -pedantic -Wall \
	  -undefined dynamic_lookup \
	  `pkg-config --cflags --libs libotr`
	strip -x $@

glirc-otr.so: glirc-otr.c
	cc -shared -o $@ $^ \
	  -pedantic -fpic -Wall \
	  `pkg-config --cflags --libs libotr`

clean:
	rm -rf *.dylib *.so *.dSYM
