#!/bin/bash

declare -a datasets=("../data/age_nli.csv" "../data/height_nli.csv" "../data/nounit_nli.csv" "../data/fahrenheit_nli.csv" "../data/celsius_nli.csv" "../data/age_reversed_nli.csv" "../data/height_reversed_nli.csv" "../data/nounit_reversed_nli.csv" "../data/fahrenheit_reversed_nli.csv" "../data/celsius_reversed_nli.csv")
for dataset in ${datasets[@]}
do
    echo "Running experiments on ${dataset}!"
    python ../python/vaguenli.py --dataset ${dataset} --device cuda:1
done
