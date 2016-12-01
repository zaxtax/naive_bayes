#!/bin/bash

echo "Running Baseline"
./nb

echo "Running Gibbs updates"
./NBHakaru 10 3 100 1
./NBHakaru 10 3 100 2
./NBHakaru 10 3 100 3
./NBHakaru 10 3 100 4
./NBHakaru 10 3 100 5
