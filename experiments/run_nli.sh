#!/bin/bash

declare -a datasets=("../data/age_nli.csv" "../data/height_nli.csv" "../data/nounit_nli.csv" "../data/fahrenheit_nli.csv" "../data/celsius_nli.csv")
for dataset in ${datasets[@]}
do
    echo "Running experiments on ${dataset}!"
    python ../python/vaguenli.py --dataset ${dataset} --device cuda:1
done