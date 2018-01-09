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

glirc-otr.dylib: glirc-otr.cpp OTR.cpp
	c++ -shared -o $@ $^ \
	  -std=c++14 \
	  -Wno-c99-extensions\
	  -pedantic -Wall \
	  -undefined dynamic_lookup \
	  `pkg-config --cflags --libs libotr`
	strip -x $@

glirc-otr.so: glirc-otr.cpp
	c++ -shared -o $@ $^ \
	  -std=c++14 \
	  -pedantic -fpic -Wall \
	  `pkg-config --cflags --libs libotr`

clean:
	rm -rf *.dylib *.so *.dSYM
