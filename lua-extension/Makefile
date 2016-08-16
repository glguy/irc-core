.PHONY: clean macosx linux

macosx: glirc-lua.dylib
linux:  glirc-lua.so

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
	  `pkg-config --cflags --libs lua-5.3`

clean:
	rm -rf glirc-lua.dylib glirc-lua.so glirc-lua.dylib.dSYM
