#!/bin/bash

echo "System,DocSize,Trial,Acc,Update.time,Init.time" > nbtimes2.csv

for d in 10 #0 400 700 1000
do
   echo "Training on 20*$d documents"
   for i in $(seq 1 10)
   do
      if [ -f nbsimp.hk ]; then
         rm nbsimp.hk nbsimp2.hk GibbsOptBucket.hs  
      fi
      start=`date +%s.%N`
      simplify naive_bayes_gibbs.hk > nbsimp.hk
      perl unsample.pl nbsimp.hk > nbsimp2.hk
      summary nbsimp2.hk -o GibbsOptBucket.hs -M GibbsOptBucket --logfloat-prelude
      ghc -O2 NaiveBayesMain.hs -o NaiveBayes
      echo "    trial $i"
      ./NaiveBayes $d $i >> nbtimes2.csv
      end=`date +%s.%N`
      echo "$end - $start" | bc -l >> nbtimes2.csv
   done
done
