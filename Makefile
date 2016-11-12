all:
	gcc -Ofast -c nb_simp.c
	ghc -O2 nb_simp.o --make NBHakaru.hs # Haskell and C
	ghc --make Main.hs -O2 -o nb # Baseline
clean:
	rm nb *.o *.hi
run:
	./NBHakaru 20 3 100 1
