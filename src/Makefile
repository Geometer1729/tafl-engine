install: build

build:
	stack ghc -- -O2 -threaded -Wall -j16 Client

clean:
	rm *.hi *.o

run: build
	./Client +RTS -N24
