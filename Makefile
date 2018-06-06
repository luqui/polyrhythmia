all:
	ghc --make -O -threaded -Werror Main 

clean:
	rm *.hi *.o

run: all
	./Main
