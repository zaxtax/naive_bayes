SANDBOX_PATH = $(shell find .cabal-sandbox -name "*-packages.conf.d")
SANDBOX_OPTIONS = -no-user-package-db -package-db $(SANDBOX_PATH)

all:
	hkc -F gibbsC -O nb_simp.hk -o nb_simp.c
	cat shim.c >> nb_simp.c
	gcc -O3 -c nb_simp.c
	ghc -O2 nb_simp.o --make NBHakaru.hs # Haskell and C
	ghc --make Main.hs -O2 -o nb # Baseline

all-sandbox:
	hkc -F gibbsC -O nb_simp.hk -o nb_simp.c
	cat shim.c >> nb_simp.c
	gcc -O3 -c nb_simp.c
	ghc --make -O2 $(SANDBOX_OPTIONS) NBHakaru.hs nb_simp.o # Haskell and C
	ghc --make -O2 $(SANDBOX_OPTIONS) Main.hs -o nb # Baseline

clean:
	rm NBHakaru nb *.o *.hi nb_simp.c *.core
run:
	sh run.sh
%.hs : %.hk
	compile $<
%.core : %.hs
	ghc -O2 -ddump-simpl -dverbose-core2core $< nb_simp.o > $@
