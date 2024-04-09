#!/bin/bash

# Set variables
model_name=$1
output_path=$2
data_path=$3
newas=$4
n=$5

# Activate virtual environment
source .venv/bin/activate

# Run Python script
python NN_test.py $model_name $output_path $data_path $newas $n $6 $7