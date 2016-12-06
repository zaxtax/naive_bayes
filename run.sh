#!/bin/bash

echo "Running Baseline"
# ./nb

echo "Running Gibbs updates"
for i in {1..10}
do
  ./NBHakaru 10 3 100 $i >> gibbstimes.csv
done
