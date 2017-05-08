#!/bin/bash

echo "System,DocSize,Trial,Acc,Update.time,Init.time" > nbtimes.csv

for d in 10 100 400 700 1000
do
   echo "Training on 20*$d documents"
   for i in $(seq 1 10)
   do
      echo "    trial $i"
      start=`date +%s.%N`
      ./GetNews $i
      ./naive_bayes.R $d $i >> nbtimes.csv
      end=`date +%s.%N`
      echo "$end - $start" | bc -l >> nbtimes.csv
   done
done
