install: build

build:
	ghc -O2 -dynamic -threaded -Wall -j16 Tests

clean:
	rm *.hi *.o

run:
	./Tests +RTS -N24
