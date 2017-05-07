#!/bin/bash

echo "System,DocSize,Trial,Acc,Update.time,Init.time" > nbtimes.csv

for d in 100 400 700 1000
do
   echo "Training on 20*$d documents"
   for i in $(seq 1 10)
   do
      echo "    trial $i"
      ghc -O2 GetNews.hs
      ./GetNews $i
      ./naive_bayes.R $d $i >> nbtimes.csv
   done
done
