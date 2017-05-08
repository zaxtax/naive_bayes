#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "./naive_bayes.sh <docsPerTopic> <trial>"
    exit
fi

start=`date +%s.%N`
./GetNews $2
./naive_bayes.R $1 $2
end=`date +%s.%N`
echo "$end - $start" | bc -l
