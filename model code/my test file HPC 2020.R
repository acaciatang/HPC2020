# CMEE 2020 HPC excercises R code HPC run code proforma

rm(list=ls()) # good practice in this instance
source("tang_HPC_2020_main.R")
# it should take a faction of a second to source your file
# if it takes longer you're using the main file to do actual simulations
# it should be used only for defining functions that will be useful for your cluster run and which will be marked automatically

# do what you like here to test your functions (this won't be marked)
# for example
species_richness(c(1,4,4,5,1,6,1))
# should return 4 when you've written the function correctly for question 1

# you may also like to use this file for playing around and debugging
# but please make sure it's all tidied up by the time it's made its way into the main.R file or other files.

speciation_rate = 0.1
size = 100
wall_time = 5
interval_rich = 1
interval_oct = 10
burn_in_generations = 20
output_file_name = "my_test_file.rda"



#put files
sftp tst116@login.cx1.hpc.ic.ac.uk
#[enter imperial password]
put tang_HPC_2020_main.R
put tang_HPC_2020_cluster.R
put clusterRun.sh
exit

#set up
Ssh -l tst116 login.cx1.hpc.ic.ac.uk
#[enter imperial password]
#mkdir tang_HPC_2020
#mv tang_HPC_2020_cluster.R $HOME/tang_HPC_2020
#cd tang_HPC_2020
#cat tang_HPC_2020_cluster.R
#module load anaconda3/personal
#anaconda-setup
#conda install r

#run
qsub -J 1-100 clusterRun.sh

#get files
tar czvf Simulation.tgz *
tar czvf Error.tgz *
tar czvf Output.tgz *

sftp tst116@login.cx1.hpc.ic.ac.uk
#[enter imperial password]
get tang_HPC_2020/Simulation.tgz
get error/Error.tgz
get output/Output.tgz
exit