.PHONY: libnotify.so

libnotify.so:
	cc -shared -o libnotify.so lua-libnotify.c \
            -Wno-deprecated-declarations \
	    -fpic \
            `pkg-config --cflags --libs libnotify` \
            `pkg-config --cflags --libs lua5.3`

clean:
	rm -f libnotify.so
