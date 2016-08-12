.PHONY: clean

sample.so: sample.c
	cc -shared -o $@ $^ -I../include

clean:
	rm -f sample.so
