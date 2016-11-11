all:
	gcc -Ofast -c nb_simp.c
	ghc -O2 nb_simp.o --make NBHakaru.hs
clean:
	rm nb *.o *.hi
run:
	./NBHakaru 20 3 100 1
