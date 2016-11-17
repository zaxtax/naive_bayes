#!/bin/bash

echo "Running Baseline"
./nb

echo "Running Gibbs update"
./NBHakaru 10 3 100 1
