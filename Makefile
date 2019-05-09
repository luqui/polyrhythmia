all:
	ghc --make -O -threaded Main 

clean:
	rm *.hi *.o

run: all
	./Main
