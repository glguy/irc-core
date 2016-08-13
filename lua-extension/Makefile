.PHONY: clean


glirc-lua.so: glirc-lua.c
	cc -g -shared -o $@ $^ -I../include \
	  -undefined dynamic_lookup \
	  `pkg-config --cflags --libs lua-5.3`

clean:
	rm -rf glirc-lua.so glirc-lua.so.dSYM
