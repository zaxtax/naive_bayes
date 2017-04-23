#!/bin/bash

echo "System,Time,DocSize,Acc,Trial" > nbtimes.csv

for d in 100 400 700 1000
do
   echo "Training on 20*$d documents"
   for i in $(seq 1 10)
   do
      echo "    trial $i"
      ./naive_bayes.R $d $i >> nbtimes.csv
   done
done
