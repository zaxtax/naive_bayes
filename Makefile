all:
	hkc -F gibbsC -O nb_simp.hk -o nb_simp.c
	cat shim.c >> nb_simp.c
	gcc -Ofast -c nb_simp.c
	ghc -O2 nb_simp.o --make NBHakaru.hs # Haskell and C
	ghc --make Main.hs -O2 -o nb # Baseline
clean:
	rm NBHakaru nb *.o *.hi nb_simp.c
run:
	sh run.sh
%.hs : %.hk
	compile $<
