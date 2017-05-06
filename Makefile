SANDBOX_PATH = $(shell find .cabal-sandbox -name "*-packages.conf.d")
SANDBOX_OPTIONS = -no-user-package-db -package-db $(SANDBOX_PATH)

all: data-file nb_simp.hk nb_simp.c
	#gcc -O3 -c gibbs.c
	summary nb_simp.hk -o GibbsOptBucket.hs -M GibbsOptBucket --logfloat-prelude
	ghc -O2 gibbs.o --make NBHakaru.hs # Haskell and C
	ghc Baseline.hs -O2 -o nb # Baseline
	ghc -O2 NaiveBayesMain.hs -o NaiveBayes

all-sandbox: data-file nb_simp.hk nb_simp.c
	#gcc -O3 -g -c gibbs.c
	summary nb_simp.hk -o GibbsOptBucket.hs -M GibbsOptBucket --logfloat-prelude
	ghc -O2 gibbs.o $(SANDBOX_OPTIONS) NBHakaru.hs # Haskell and C
	ghc Baseline.hs -O2 $(SANDBOX_OPTIONS) -o nb # Baseline
	ghc -O2 NaiveBayesMain.hs $(SANDBOX_OPTIONS) -o NaiveBayes

data-file:
	bash ./download-data.sh

nb_simp.hk:
	simplify naive_bayes_gibbs.hk > nb_simp.hk

nb_simp.c: nb_simp.hk
	hkc -F gibbsC -O nb_simp.hk -o nb_simp.c

clean:
	rm NBHakaru nb *.o *.hi nb_simp.hk nb_simp.c *.core NaiveBayes

distclean: clean
	rm -rf 20news-19997.tar.gz 20_newsgroups

run:
	sh run.sh
	Rscript naive_bayes.R

%.hs : %.hk
	compile $<

%.core : %.hs
	ghc -O2 -ddump-simpl -dverbose-core2core -dppr-cols200 $< nb_simp.o > $@
