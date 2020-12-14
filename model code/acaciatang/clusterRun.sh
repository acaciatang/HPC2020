#!/bin/bash
#PBS -lwalltime=12:00:00
#PBS -lselect=1:ncpus=1:mem=1gb
module load anaconda3/personal
echo "R is about to run"
R --vanilla < $HOME/acaciatang_HPC_2020_cluster.R
mv Simulation* $HOME/acaciatang_HPC_2020
echo "R has finished running"

#done