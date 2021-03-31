install: build

build:
	stack ghc -- -O2 -threaded -Wall -j16 Client

clean:
	rm *.hi *.o

run:
	./Client +RTS -N24
