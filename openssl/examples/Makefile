GHCFLAGS = -O2

build:
	ghc $(GHCFLAGS) --make GenRSAKey
	ghc $(GHCFLAGS) --make HelloWorld
	ghc $(GHCFLAGS) --make PKCS7
	ghc $(GHCFLAGS) --make -threaded Server

run: build
	./PKCS7
#	./HelloWorld

clean:
	rm -f HelloWorld GenRSAKey PKCS7 Server *.hi *.o

.PHONY: build run clean
