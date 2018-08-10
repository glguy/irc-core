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

macos: glirc-lua.bundle
linux: glirc-lua.so

OBJS = glirc-lua.o glirc-marshal.o glirc-lib.o
EXE  = $(HOME)/.cabal/bin/glirc2
CC   = cc
LUA  = lua-5.3

%.o: %.c
	$(CC) -c -o $@ $^ -I../include \
	  -std=c99 -fpic -pedantic -Wall -O \
	  `pkg-config --cflags $(LUA)`

glirc-lua.bundle: $(OBJS)
	$(CC) -bundle -o $@ $^ \
	  -Wl,-exported_symbol,_extension \
	  -Wl,-bundle_loader,$(EXE) \
	  `pkg-config --libs $(LUA)`

glirc-lua.so: $(OBJS)
	$(CC) -shared -o $@ $^ -I../include \
	  `pkg-config --libs $(LUA)`

# glirc-lua-debug.dylib: $(OBJS)
# 	cc -shared -o $@ $^ -I../include \
# 	  -pedantic -Wall \
# 	  -undefined dynamic_lookup \
# 	  -L/Users/emertens/Source/galua/galua-c/inplace/lib \
# 	  -I/Users/emertens/Source/galua/galua-c/inplace/include/galua \
# 	  -lgalua-dbg -liconv -lz

clean:
	rm -rf *.bundle *.o *.so *.dSYM
