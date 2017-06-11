.PHONY: help clean macos linux

help:
	@echo 'Please use "make macos" or "make linux"'

macos: glirc-otr.dylib
linux:  glirc-otr.so

glirc-otr.dylib: glirc-otr.c
	cc -shared -o $@ $^ -I../include \
	  -pedantic -Wall \
	  -undefined dynamic_lookup \
	  `pkg-config --cflags --libs libotr`

glirc-otr.so: glirc-otr.c
	cc -shared -o $@ $^ -I../include \
	  -pedantic -fpic -Wall \
	  `pkg-config --cflags --libs libotr`

clean:
	rm -rf *.dylib *.so *.dSYM
