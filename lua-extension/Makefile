.PHONY: help clean macos linux

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

macos: glirc-lua.dylib
linux: glirc-lua.so

glirc-lua.dylib: glirc-lua.c
	cc -shared -o $@ $^ -I../include \
	  -pedantic -Wall \
	  -undefined dynamic_lookup \
	  `pkg-config --cflags --libs lua-5.3`

glirc-lua.so: glirc-lua.c
	cc -shared -o $@ $^ -I../include \
	  -pedantic -fpic -Wall \
	  `pkg-config --cflags --libs lua-5.3`

glirc-lua-debug.dylib: glirc-lua.c
	cc -shared -o $@ $^ -I../include \
	  -pedantic -Wall \
	  -undefined dynamic_lookup \
	  -L/Users/emertens/Source/galua/galua-c/inplace/lib \
	  -I/Users/emertens/Source/galua/galua-c/inplace/include/galua \
	  -lgalua-dbg -liconv -lz

clean:
	rm -rf *.dylib *.so *.dSYM
