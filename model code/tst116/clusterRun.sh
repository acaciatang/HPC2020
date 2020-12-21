#!/bin/bash
#PBS -lwalltime=12:00:00
#PBS -lselect=1:ncpus=1:mem=1gb
module load anaconda3/personal # allows R to be run with HPC
echo "R is about to run"
R --vanilla < $HOME/tst116_HPC_2020_cluster.R # run simulation
mv Simulation* $HOME/tst116_HPC_2020 # move files to folder
echo "R has finished running"

#done