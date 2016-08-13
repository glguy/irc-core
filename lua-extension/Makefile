.PHONY: clean


glirc-lua.so: glirc-lua.c
#	cc -g -shared -o $@ $^ -I../include \
#	-L/Users/emertens/Source/galua/galua-c/inplace/lib \
#	-I/Users/emertens/Source/galua/galua-c/inplace/include/galua \
#	-lgalua-dbg -liconv -lz
	cc -g -shared -o $@ $^ -I../include `pkg-config --cflags --libs lua-5.3` -undefined dynamic_lookup

clean:
	rm -f glirc-lua.so
