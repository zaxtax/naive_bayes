all:
	ghc --make Main.hs -O2 -o nb
clean:
	rm nb *.o *.hi
